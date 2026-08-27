
from pathlib import Path
import html
import json
import pickle
import re
import traceback
 
import gradio as gr
import numpy as np
import pandas as pd
import sklearn.compose._column_transformer as sklearn_column_transformer
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.preprocessing import OneHotEncoder, StandardScaler

PDF_IMPORT_ERROR = None
try:
    from pypdf import PdfReader
except Exception as exc:  # pragma: no cover - defensive import guard
    PdfReader = None
    PDF_IMPORT_ERROR = str(exc)
 
# --- Dense embedding + vector search + LLM-generation stack for the RAG tab ---
# Same base embedding model taught in RAG_Part_1-3 (sentence-transformers/all-MiniLM-L6-v2),
# loaded through the sentence-transformers wrapper (correct attention-masked mean pooling)
# instead of the manual AutoModel + naive mean-pool shown in class. FAISS provides the
# vector index (cosine similarity via inner product on normalized vectors), and the OpenAI
# Chat Completions API provides the "generated response" step. All three imports are
# optional at runtime: if any of them fail to load (e.g., no internet at build time), the
# app logs the failure and automatically falls back to the original TF-IDF lexical
# retriever and an extractive (non-generated) answer, so the app never crashes.
EMBEDDING_MODEL_NAME = "sentence-transformers/all-MiniLM-L6-v2"
EMBEDDING_IMPORT_ERROR = None
try:
    from sentence_transformers import SentenceTransformer
    import faiss
except Exception as exc:  # pragma: no cover - defensive import guard
    SentenceTransformer = None
    faiss = None
    EMBEDDING_IMPORT_ERROR = str(exc)
 
OPENAI_IMPORT_ERROR = None
try:
    from openai import OpenAI
except Exception as exc:  # pragma: no cover - defensive import guard
    OpenAI = None
    OPENAI_IMPORT_ERROR = str(exc)


if not hasattr(sklearn_column_transformer, "_RemainderColsList"):
    class _RemainderColsList(list):
        pass

    _RemainderColsList.__module__ = sklearn_column_transformer.__name__
    sklearn_column_transformer._RemainderColsList = _RemainderColsList
 
 
RANDOM_STATE = 42
BASE_DIR = Path(__file__).resolve().parent
 
 
def resolve_folder(name):
    for candidate in [BASE_DIR / name, BASE_DIR.parent / "app" / name, BASE_DIR.parent / name]:
        if candidate.exists():
            return candidate
    return BASE_DIR / name
 
 
DATA_DIR = resolve_folder("data")
ARTIFACT_DIR = resolve_folder("artifacts")
PDF_DIR = resolve_folder("pdfs")
DATA_PATH = DATA_DIR / "diabetic_data.csv"
MODEL_PATH = ARTIFACT_DIR / "diabetes_readmission_pipeline.pkl"
SAMPLE_PATH = ARTIFACT_DIR / "sample_scored_patients.csv"
SUMMARY_PATH = ARTIFACT_DIR / "model_summary.json"
FINE_TUNED_COMPARISON_PATH = ARTIFACT_DIR / "fine_tuned_model_comparison.csv"
 
DEFAULT_THRESHOLD = 0.30
DEFAULT_CAPACITY = 500
DEFAULT_READMISSION_COST = 15000
DEFAULT_INTERVENTION_COST = 300
DEFAULT_EFFECTIVENESS = 0.10
 
SMU_BLUE = "#354CA1"
SMU_RED = "#CC0035"
SMU_NAVY = "#1F2A44"
SMU_LIGHT = "#F8FAFC"
SMU_BORDER = "#D9DFEA"
SMU_TEXT = "#202124"
 
 
def icd_chapter(code):
    if pd.isna(code):
        return np.nan
 
    code = str(code).strip()
    if code.startswith("V"):
        return 18
    if code.startswith("E"):
        return 19
 
    try:
        num = float(code)
    except ValueError:
        return np.nan
 
    if num < 140:
        return 1
    if num < 240:
        return 2
    if num < 280:
        return 3
    if num < 290:
        return 4
    if num < 320:
        return 5
    if num < 390:
        return 6
    if num < 460:
        return 7
    if num < 520:
        return 8
    if num < 580:
        return 9
    if num < 630:
        return 10
    if num < 680:
        return 11
    if num < 710:
        return 12
    if num < 740:
        return 13
    if num < 760:
        return 14
    if num < 780:
        return 15
    if num < 800:
        return 16
    if num < 1000:
        return 17
    return np.nan
 
 
def discharge_group(disposition_id):
    groups = {
        1: "Home",
        6: "Home health",
        2: "Short-term hospital transfer",
        3: "Skilled nursing facility",
        4: "Intermediate care facility",
        5: "Other inpatient facility",
        7: "Left AMA",
        12: "Expected outpatient return",
        22: "Rehab facility",
        23: "Long-term care hospital",
        24: "Medicaid nursing facility",
    }
    return groups.get(disposition_id, "Other/unknown")
 
 
def admission_type_group(admission_type_id):
    groups = {
        1: "Emergency",
        2: "Urgent",
        3: "Elective",
        4: "Newborn",
        5: "Not available",
        6: "Null",
        7: "Trauma center",
        8: "Not mapped",
    }
    return groups.get(admission_type_id, "Other/unknown")
 
 
def admission_source_group(source_id):
    if source_id == 7:
        return "Emergency room"
    if source_id in [1, 2, 3]:
        return "Referral"
    if source_id in [4, 5, 6, 10, 18, 19, 22, 25, 26]:
        return "Transfer/continuing care"
    if source_id in [9, 17, 20, 21]:
        return "Unknown/not available"
    return "Other"
 
 
def clean_diabetes_data(df):
    df = df.copy().replace("?", np.nan)
    df = df[~df["discharge_disposition_id"].isin([11, 13, 14, 19, 20, 21])].copy()
 
    df["discharge_group"] = df["discharge_disposition_id"].apply(discharge_group)
    df["admission_type_group"] = df["admission_type_id"].apply(admission_type_group)
    df["admission_source_group"] = df["admission_source_id"].apply(admission_source_group)
 
    drop_cols = [
        "encounter_id",
        "patient_nbr",
        "weight",
        "payer_code",
        "examide",
        "citoglipton",
        "troglitazone",
        "acetohexamide",
        "tolbutamide",
        "tolazamide",
        "glimepiride-pioglitazone",
        "metformin-rosiglitazone",
        "metformin-pioglitazone",
    ]
    df = df.drop(columns=[col for col in drop_cols if col in df.columns])
    df = df[df["gender"] != "Unknown/Invalid"].copy()
 
    age_map = {
        "[0-10)": 0,
        "[10-20)": 1,
        "[20-30)": 2,
        "[30-40)": 3,
        "[40-50)": 4,
        "[50-60)": 5,
        "[60-70)": 6,
        "[70-80)": 7,
        "[80-90)": 8,
        "[90-100)": 9,
    }
    df["age_ordinal"] = df["age"].map(age_map)
 
    med_cols = [
        "metformin",
        "repaglinide",
        "nateglinide",
        "chlorpropamide",
        "glimepiride",
        "glipizide",
        "glyburide",
        "pioglitazone",
        "rosiglitazone",
        "acarbose",
        "miglitol",
        "insulin",
        "glyburide-metformin",
        "glipizide-metformin",
    ]
    med_map = {"No": 0, "Steady": 1, "Down": 2, "Up": 3}
    for col in med_cols:
        if col in df.columns:
            df[col] = df[col].map(med_map)
 
    active_med_cols = [col for col in med_cols if col in df.columns]
    df["num_active_diabetes_meds"] = (df[active_med_cols].fillna(0) > 0).sum(axis=1)
    df["medication_change_flag"] = (df["change"] == "Ch").astype(int)
    df["total_prior_visits"] = df[
        ["number_outpatient", "number_emergency", "number_inpatient"]
    ].sum(axis=1)
    df["had_prior_utilization"] = (df["total_prior_visits"] > 0).astype(int)
    df["tests_per_day"] = df["num_lab_procedures"] / df["time_in_hospital"].clip(lower=1)
    df["medications_per_day"] = df["num_medications"] / df["time_in_hospital"].clip(lower=1)
 
    for diag_col in ["diag_1", "diag_2", "diag_3"]:
        df[f"{diag_col}_chapter"] = df[diag_col].apply(icd_chapter)
        df = df.drop(columns=[diag_col])
 
    if "readmitted" in df.columns:
        df["readmitted_30"] = (df["readmitted"] == "<30").astype(int)
 
    return df
 
 
def train_model_from_data():
    if not DATA_PATH.exists():
        raise FileNotFoundError(
            "No saved model or source dataset is available. Add artifacts/diabetes_readmission_pipeline.pkl."
        )
 
    raw = pd.read_csv(DATA_PATH)
    df = clean_diabetes_data(raw)
    y = df["readmitted_30"]
    x = df.drop(columns=["readmitted", "readmitted_30"])
 
    numeric_features = x.select_dtypes(include=["int64", "float64"]).columns.tolist()
    categorical_features = x.select_dtypes(include=["object", "category"]).columns.tolist()
 
    x_train, _, y_train, _ = train_test_split(
        x, y, test_size=0.2, stratify=y, random_state=RANDOM_STATE
    )
 
    numeric_pipe = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler()),
        ]
    )
    try:
        onehot = OneHotEncoder(handle_unknown="ignore", sparse_output=True)
    except TypeError:
        onehot = OneHotEncoder(handle_unknown="ignore", sparse=True)
    categorical_pipe = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="most_frequent")),
            ("onehot", onehot),
        ]
    )
    preprocessor = ColumnTransformer(
        transformers=[
            ("num", numeric_pipe, numeric_features),
            ("cat", categorical_pipe, categorical_features),
        ]
    )
    model = Pipeline(
        steps=[
            ("preprocess", preprocessor),
            (
                "model",
                LogisticRegression(
                    max_iter=1000,
                    class_weight="balanced",
                    random_state=RANDOM_STATE,
                ),
            ),
        ]
    )
    model.fit(x_train, y_train)
    return {
        "model": model,
        "feature_columns": x.columns.tolist(),
        "support_levels": support_levels(),
    }
 
 
def support_levels():
    return {
        "Routine": "Routine discharge support",
        "Enhanced": "Follow-up call + appointment coordination",
        "Intensive": "Intensive nurse care management + medication review",
    }
 
 
def load_model_bundle():
    if MODEL_PATH.exists():
        with open(MODEL_PATH, "rb") as f:
            return pickle.load(f)
    return train_model_from_data()
 
 
def load_summary():
    if SUMMARY_PATH.exists():
        with open(SUMMARY_PATH) as f:
            return json.load(f)
    return {
        "model_name": "Balanced Logistic Regression readmission-risk pipeline",
        "metrics": {},
        "limitations": [
            "Prototype only; validate locally before operational use.",
            "Predictions support human review and should not replace clinical judgment.",
        ],
    }
 
 
MODEL_BUNDLE = load_model_bundle()
MODEL_SUMMARY = load_summary()
 
 
def load_default_rows():
    if SAMPLE_PATH.exists():
        return pd.read_csv(SAMPLE_PATH).head(500)
    if DATA_PATH.exists():
        return pd.read_csv(DATA_PATH).head(2000)
    return pd.DataFrame()
 
 
def read_uploaded_csv(file_obj):
    if file_obj is None:
        return load_default_rows(), "sample patient list"
 
    file_path = file_obj if isinstance(file_obj, (str, Path)) else file_obj.name
    return pd.read_csv(file_path), Path(file_path).name
 
 
def prepare_features(input_df):
    if {"readmitted", "diag_1", "diag_2", "diag_3"}.issubset(input_df.columns):
        prepared = clean_diabetes_data(input_df)
    else:
        prepared = input_df.copy()
 
    feature_columns = MODEL_BUNDLE["feature_columns"]
    for col in feature_columns:
        if col not in prepared.columns:
            prepared[col] = np.nan
    return prepared[feature_columns], prepared
 
 
def assign_support_level(score, threshold):
    medium_threshold = max(0.01, threshold * 0.70)
    if score >= threshold:
        return "Intensive nurse care management + medication review"
    if score >= medium_threshold:
        return "Follow-up call + appointment coordination"
    return "Routine discharge support"
 
 
def risk_badge(score, threshold):
    medium_threshold = max(0.01, threshold * 0.70)
    if score >= threshold:
        return "High priority"
    if score >= medium_threshold:
        return "Watch list"
    return "Routine"
 
 
def score_dataframe(raw_df, threshold):
    if raw_df.empty:
        return pd.DataFrame()
 
    x, prepared = prepare_features(raw_df)
    scores = MODEL_BUNDLE["model"].predict_proba(x)[:, 1]
 
    output = prepared.copy()
    output["risk_score"] = scores
    output["priority"] = output["risk_score"].apply(lambda value: risk_badge(value, threshold))
    output["recommended_support"] = output["risk_score"].apply(
        lambda value: assign_support_level(value, threshold)
    )
    output["rank"] = output["risk_score"].rank(method="first", ascending=False).astype(int)
    return output.sort_values("risk_score", ascending=False)
 
 
def money(value):
    return f"${value:,.0f}"
 
 
def pct(value):
    if pd.isna(value):
        return "n/a"
    return f"{value:.1%}"
 
 
def metric_card(label, value, detail=""):
    return f"""
    <div class="metric-card">
        <div class="metric-label">{label}</div>
        <div class="metric-value">{value}</div>
        <div class="metric-detail">{detail}</div>
    </div>
    """


def model_metric_value(metrics, calibrated, name):
    return calibrated.get(name, metrics.get(name))


def build_score_summary(scored_df, threshold, source_name):
    if scored_df.empty:
        return """
        <div class="result-card">
            <div class="result-label">No Data</div>
            <div class="result-value">Upload a CSV</div>
            <div class="score-value">The app can score raw UCI diabetes rows or already-cleaned rows.</div>
        </div>
        """
 
    high = int((scored_df["risk_score"] >= threshold).sum())
    watch = int(
        (
            (scored_df["risk_score"] >= max(0.01, threshold * 0.70))
            & (scored_df["risk_score"] < threshold)
        ).sum()
    )
    avg_score = float(scored_df["risk_score"].mean())
    top_score = float(scored_df["risk_score"].max())
    card_class = "high-result" if high else "safe-result"
 
    return f"""
    <div class="result-card {card_class}">
        <div class="result-label">Selected</div>
        <div class="result-value">{high:,}</div>
        <div class="risk-value">above {threshold:.0%} risk</div>
        <div class="score-value">
            {source_name} • Watch {watch:,} • Avg {avg_score:.1%} • Top {top_score:.1%}
        </div>
    </div>
    """
 
 
def score_patients(file_obj, threshold, top_n):
    raw, source_name = read_uploaded_csv(file_obj)
    scored = score_dataframe(raw, threshold)
    summary = build_score_summary(scored, threshold, source_name)
 
    if scored.empty:
        return summary, pd.DataFrame({"message": ["Upload a CSV or include artifacts/sample_scored_patients.csv."]})
 
    display_cols = [
        "rank",
        "risk_score",
        "priority",
        "recommended_support",
        "age",
        "race",
        "gender",
        "time_in_hospital",
        "number_inpatient",
        "number_emergency",
        "number_outpatient",
        "discharge_group",
    ]
    display_cols = [col for col in display_cols if col in scored.columns]
    table = scored.head(int(top_n))[display_cols].copy()
    if "risk_score" in table.columns:
        table["risk_score"] = table["risk_score"].map(lambda value: f"{value:.1%}")
    return summary, table
 
 
def estimate_intervention_value(
    scores,
    threshold,
    capacity,
    selection_mode,
    target_patients,
    readmission_cost,
    intervention_cost,
    effectiveness,
):
    sorted_scores = np.sort(scores)[::-1]
    if selection_mode == "Top N highest-risk patients":
        selected = sorted_scores[: int(target_patients)]
    else:
        selected = sorted_scores[sorted_scores >= threshold]
        if capacity > 0:
            selected = selected[: int(capacity)]
 
    patients_targeted = len(selected)
    expected_readmissions = float(selected.sum())
    expected_avoided = expected_readmissions * effectiveness
    gross_savings = expected_avoided * readmission_cost
    total_cost = patients_targeted * intervention_cost
    net_value = gross_savings - total_cost
    break_even = (
        total_cost / (expected_readmissions * readmission_cost)
        if expected_readmissions and readmission_cost
        else np.nan
    )
 
    return {
        "patients_targeted": patients_targeted,
        "selection_mode": selection_mode,
        "expected_readmissions_without_intervention": expected_readmissions,
        "expected_readmissions_avoided": expected_avoided,
        "gross_avoided_readmission_cost": gross_savings,
        "total_intervention_cost": total_cost,
        "estimated_net_value": net_value,
        "break_even_effectiveness": break_even,
    }
 
 
def scenario_planner(
    file_obj,
    threshold,
    capacity,
    selection_mode,
    target_patients,
    readmission_cost,
    intervention_cost,
    effectiveness,
):
    raw, source_name = read_uploaded_csv(file_obj)
    scored = score_dataframe(raw, threshold)
 
    if scored.empty:
        empty = pd.DataFrame({"message": ["Upload a CSV or include artifacts/sample_scored_patients.csv."]})
        return "", empty
 
    estimate = estimate_intervention_value(
        scored["risk_score"].to_numpy(),
        threshold,
        capacity,
        selection_mode,
        target_patients,
        readmission_cost,
        intervention_cost,
        effectiveness,
    )
    result_class = "value-positive" if estimate["estimated_net_value"] >= 0 else "value-negative"
    result_html = f"""
    <div class="result-card {result_class}">
        <div class="result-label">Net Value</div>
        <div class="result-value">{money(estimate["estimated_net_value"])}</div>
        <div class="risk-value">{estimate["patients_targeted"]:,} patients targeted</div>
        <div class="score-value">
            Avoided {estimate["expected_readmissions_avoided"]:.1f} • Break-even {pct(estimate["break_even_effectiveness"])} • {source_name}
        </div>
    </div>
    """
    table = pd.DataFrame(
        [
            {
                "Patients targeted": estimate["patients_targeted"],
                "Selection mode": estimate["selection_mode"],
                "Expected readmissions without intervention": round(
                    estimate["expected_readmissions_without_intervention"], 1
                ),
                "Expected readmissions avoided": round(estimate["expected_readmissions_avoided"], 1),
                "Gross avoided readmission cost": money(estimate["gross_avoided_readmission_cost"]),
                "Total intervention cost": money(estimate["total_intervention_cost"]),
                "Estimated net value": money(estimate["estimated_net_value"]),
                "Break-even effectiveness": pct(estimate["break_even_effectiveness"]),
            }
        ]
    )
    return result_html, table
 
 
PDF_SOURCE_LABELS = {
    "HRRP_Formula.pdf": "CMS HRRP payment reduction formula",
    "goinghomeguide.pdf": "AHRQ Taking Care of Myself discharge guide",
    "dischargeprep.pdf": "AHRQ RED Discharge Preparation Workbook",
    "A Reengineered Hospital Discharge Program to Decrease Rehospitalization.pdf": (
        "Project RED randomized trial"
    ),
}
RAG_CACHE = None
EMBEDDING_MODEL = None
 
 
def clean_pdf_text(text):
    text = re.sub(r"\s+", " ", text or "").strip()
    return text.replace("(cid:0)", "-").replace("(cid:1)", "")
 
 
def split_text_into_chunks(text, max_chars=950, overlap=160):
    text = clean_pdf_text(text)
    if not text:
        return []
 
    chunks = []
    start = 0
    while start < len(text):
        end = min(start + max_chars, len(text))
        if end < len(text):
            sentence_break = max(text.rfind(". ", start, end), text.rfind("; ", start, end))
            if sentence_break > start + 350:
                end = sentence_break + 1
        chunk = text[start:end].strip()
        if len(chunk) > 120:
            chunks.append(chunk)
        if end >= len(text):
            break
        start = max(0, end - overlap)
    return chunks
 
 
def load_pdf_chunks():
    if PdfReader is None:
        return [
            {
                "text": f"PDF reader unavailable. Install pypdf to enable PDF retrieval. Details: {PDF_IMPORT_ERROR}",
                "source": "pypdf",
                "label": "PDF reader unavailable",
                "page": 0,
            }
        ]

    chunks = []
    for pdf_path in sorted(PDF_DIR.glob("*.pdf")):
        try:
            reader = PdfReader(str(pdf_path))
        except Exception as exc:
            chunks.append(
                {
                    "text": f"Could not read {pdf_path.name}: {exc}",
                    "source": pdf_path.name,
                    "label": PDF_SOURCE_LABELS.get(pdf_path.name, pdf_path.stem),
                    "page": 0,
                }
            )
            continue
 
        for page_index, page in enumerate(reader.pages, start=1):
            page_text = clean_pdf_text(page.extract_text() or "")
            for chunk in split_text_into_chunks(page_text):
                chunks.append(
                    {
                        "text": chunk,
                        "source": pdf_path.name,
                        "label": PDF_SOURCE_LABELS.get(pdf_path.name, pdf_path.stem),
                        "page": page_index,
                    }
                )
    return chunks
 
 
def load_embedding_model():
    """Lazily load and cache the sentence-transformer embedding model."""
    global EMBEDDING_MODEL
    if EMBEDDING_MODEL is not None:
        return EMBEDDING_MODEL
    if SentenceTransformer is None:
        return None
    try:
        EMBEDDING_MODEL = SentenceTransformer(EMBEDDING_MODEL_NAME)
    except Exception:
        EMBEDDING_MODEL = False  # sentinel: tried and failed, don't retry every call
    return EMBEDDING_MODEL if EMBEDDING_MODEL else None
 
 
def get_rag_index():
    global RAG_CACHE
    if RAG_CACHE is not None:
        return RAG_CACHE
 
    chunks = load_pdf_chunks()
    if not chunks:
        RAG_CACHE = {
            "chunks": [],
            "vectorizer": None,
            "matrix": None,
            "faiss_index": None,
            "retrieval_mode": "none",
        }
        return RAG_CACHE
 
    # Lexical fallback retriever (TF-IDF) - always built, cheap, and used automatically
    # if the dense embedding model can't be loaded (e.g. no internet at cold start).
    vectorizer = TfidfVectorizer(
        stop_words="english",
        ngram_range=(1, 2),
        min_df=1,
        max_features=15000,
    )
    matrix = vectorizer.fit_transform([chunk["text"] for chunk in chunks])
 
    # Primary retriever: dense embeddings (all-MiniLM-L6-v2) indexed with FAISS.
    faiss_index = None
    embedding_model = load_embedding_model()
    if embedding_model is not None and faiss is not None:
        try:
            embeddings = embedding_model.encode(
                [chunk["text"] for chunk in chunks],
                convert_to_numpy=True,
                normalize_embeddings=True,  # so inner product == cosine similarity
                show_progress_bar=False,
            ).astype("float32")
            faiss_index = faiss.IndexFlatIP(embeddings.shape[1])
            faiss_index.add(embeddings)
        except Exception:
            faiss_index = None
 
    retrieval_mode = "dense_embeddings" if faiss_index is not None else "tfidf_fallback"
    RAG_CACHE = {
        "chunks": chunks,
        "vectorizer": vectorizer,
        "matrix": matrix,
        "faiss_index": faiss_index,
        "retrieval_mode": retrieval_mode,
    }
    return RAG_CACHE
 
 
def retrieve_pdf_chunks(question, top_k=5):
    index = get_rag_index()
    if not index["chunks"]:
        return [], index.get("retrieval_mode", "none")
 
    if index["faiss_index"] is not None:
        embedding_model = load_embedding_model()
        query_embedding = embedding_model.encode(
            [question], convert_to_numpy=True, normalize_embeddings=True, show_progress_bar=False
        ).astype("float32")
        # Search the full index so every chunk gets a similarity score; the per-source
        # diversity cap and keyword boosts below then pick the top_k from that ranking.
        similarity, order = index["faiss_index"].search(query_embedding, len(index["chunks"]))
        scores = np.zeros(len(index["chunks"]), dtype="float32")
        scores[order[0]] = similarity[0]
    else:
        query_vector = index["vectorizer"].transform([question])
        scores = cosine_similarity(query_vector, index["matrix"]).ravel()
 
    query_lower = question.lower()
    boosted_scores = scores.copy()
    for idx, chunk in enumerate(index["chunks"]):
        source = chunk["source"]
        if source == "dischargeprep.pdf" and any(
            word in query_lower
            for word in ["follow", "appointment", "medication", "medicine", "teaching", "transportation"]
        ):
            boosted_scores[idx] += 0.08
        if source == "goinghomeguide.pdf" and any(
            word in query_lower
            for word in ["home", "patient", "medicine", "doctor", "call", "appointment", "warning"]
        ):
            boosted_scores[idx] += 0.06
        if source == "A Reengineered Hospital Discharge Program to Decrease Rehospitalization.pdf" and any(
            word in query_lower
            for word in ["evidence", "trial", "pharmacist", "follow-up", "followup", "rehospitalization"]
        ):
            boosted_scores[idx] += 0.06
        if source == "HRRP_Formula.pdf" and any(
            word in query_lower
            for word in ["cost", "payment", "financial", "business", "penalty", "hrrp"]
        ):
            boosted_scores[idx] += 0.08
 
    top_indices = boosted_scores.argsort()[::-1]
 
    results = []
    source_counts = {}
    for idx in top_indices:
        if boosted_scores[idx] <= 0:
            continue
        source = index["chunks"][idx]["source"]
        if source_counts.get(source, 0) >= 2:
            continue
        result = dict(index["chunks"][idx])
        result["score"] = float(scores[idx])
        results.append(result)
        source_counts[source] = source_counts.get(source, 0) + 1
        if len(results) >= top_k:
            break
    return results, index.get("retrieval_mode", "none")
 
 
def recommendation_bullets(retrieved):
    sources = {item["source"] for item in retrieved}
    bullets = []
 
    if "dischargeprep.pdf" in sources:
        bullets.append(
            "Use the RED Discharge Preparation Workbook as the staff workflow: confirm diagnoses, follow-up appointments, transportation, medication reconciliation, pending labs, patient teaching, and who will follow up."
        )
    if "goinghomeguide.pdf" in sources:
        bullets.append(
            "Give the patient a clear take-home plan covering medicines, appointments, warning signs, who to call, diet/activity instructions, and questions for the next doctor visit."
        )
    if "A Reengineered Hospital Discharge Program to Decrease Rehospitalization.pdf" in sources:
        bullets.append(
            "For high-risk patients, prioritize RED-style services: discharge advocate support, appointment coordination, medication reconciliation, individualized instructions, and a pharmacist follow-up call within a few days after discharge."
        )
    if "HRRP_Formula.pdf" in sources:
        bullets.append(
            "For the business case, connect intervention targeting to expected readmissions avoided and CMS readmission/payment exposure rather than reporting model accuracy alone."
        )
 
    if not bullets:
        bullets.append(
            "Use the retrieved discharge-planning guidance to turn the model score into a human-reviewed care-management action."
        )
 
    return bullets
 
 
RAG_SYSTEM_PROMPT = (
    "You are a decision-support assistant for a hospital transitional-care / care-management "
    "team working on diabetes discharge planning. Answer ONLY using the numbered source excerpts "
    "provided below, which come from CMS and AHRQ discharge-planning and readmission-reduction "
    "documents. If the excerpts do not contain enough information to answer, say so explicitly "
    "instead of guessing. Write for a care-management leader, not a technical audience: be concise "
    "(4-6 sentences or short bullets), name which source number backs each claim, and end with the "
    "concrete action a care manager should take. Do not give clinical diagnoses or treatment orders - "
    "this is operational/discharge-planning guidance only, and it should support human review, not "
    "replace clinical judgment."
)
 
 
def build_rag_context(retrieved):
    lines = []
    for position, item in enumerate(retrieved, start=1):
        lines.append(f"[Source {position}: {item['label']}, page {item['page']}]\n{item['text']}")
    return "\n\n".join(lines)
 
 
def generate_llm_answer(question, retrieved, api_key):
    """Prompt design + generated response step of the RAG pipeline.
 
    Sends the retrieved, source-grounded context to an LLM (OpenAI Chat Completions) and
    returns a generated natural-language answer. Returns (answer_text, error_message); on
    any failure, answer_text is None and error_message explains why, so the caller can fall
    back to the extractive summary and the app can be transparent about the failure mode
    instead of crashing or silently producing an empty response.
    """
    if not api_key:
        return None, "no_api_key"
    if OpenAI is None:
        return None, f"openai package unavailable ({OPENAI_IMPORT_ERROR})"
 
    context = build_rag_context(retrieved)
    try:
        client = OpenAI(api_key=api_key)
        response = client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[
                {"role": "system", "content": RAG_SYSTEM_PROMPT},
                {"role": "user", "content": f"Source excerpts:\n\n{context}\n\nQuestion: {question}"},
            ],
            max_tokens=400,
            temperature=0.2,
        )
        answer = response.choices[0].message.content.strip()
        return answer, None
    except Exception as exc:  # network issues, bad/rotated key, rate limits, etc.
        return None, f"{type(exc).__name__}: {exc}"
 
 
def guidance_answer(question, api_key=None):
    question = (question or "").strip()
    if not question:
        question = "What discharge support should high-risk diabetes patients receive?"
    api_key = (api_key or "").strip()
 
    retrieved, retrieval_mode = retrieve_pdf_chunks(question, top_k=5)
    if not retrieved:
        return f"""
        <div class="guidance-card guidance-empty">
            <div class="result-label">Guidance</div>
            <h2>No sources found</h2>
            <p class="small-note">Check the PDFs in <code>{html.escape(str(PDF_DIR))}</code>.</p>
        </div>
        """
 
    retrieval_label = "FAISS" if retrieval_mode == "dense_embeddings" else "TF-IDF"
 
    llm_answer, llm_error = generate_llm_answer(question, retrieved, api_key)
 
    if llm_answer:
        formatted_answer = html.escape(llm_answer).replace("\n", "<br>")
        answer_block = f"""
        <div class="guidance-section">
            <div class="section-kicker">Answer</div>
            <div class="answer-box">{formatted_answer}</div>
        </div>
        """
        mode_note = "LLM answer grounded in retrieved sources."
    else:
        bullets = recommendation_bullets(retrieved)
        bullet_html = "".join(f"<li>{html.escape(item)}</li>" for item in bullets)
        answer_block = f"""
        <div class="guidance-section">
            <div class="section-kicker">Action</div>
            <ul class="action-list">{bullet_html}</ul>
        </div>
        """
        if llm_error == "no_api_key":
            mode_note = "Retrieval-only summary. Add a key for an LLM answer."
        else:
            mode_note = f"LLM unavailable ({html.escape(str(llm_error))}); showing retrieved-source summary."
 
    source_cards = []
    seen = set()
    for item in retrieved:
        key = (item["source"], item["page"])
        if key in seen:
            continue
        seen.add(key)
        snippet = item["text"][:520].strip()
        if len(item["text"]) > 520:
            snippet += "..."
        source_cards.append(
            f"""
            <div class="source-card">
                <div class="source-title">{html.escape(item["label"])}</div>
                <div class="source-page">Page {item["page"]}</div>
                <div class="source-snippet">{html.escape(snippet)}</div>
            </div>
            """
        )
 
    return f"""
    <div class="guidance-card">
        <div class="result-label">Guidance</div>
        <h2>{html.escape(question)}</h2>
        {answer_block}
        <div class="guidance-section">
            <div class="section-kicker">Sources</div>
            <div class="source-grid">{"".join(source_cards)}</div>
        </div>
        <div class="guidance-note">{html.escape(retrieval_label)} retrieval. {html.escape(mode_note)} Decision support only.</div>
    </div>
    """
 
 
def model_metrics_html():
    metrics = MODEL_SUMMARY.get("metrics", {})
    calibrated = MODEL_SUMMARY.get("calibrated_capacity_metrics", {})
    precision_at_500 = model_metric_value(metrics, calibrated, "precision_at_500")
    recall_at_500 = model_metric_value(metrics, calibrated, "recall_at_500")
    lift_at_500 = model_metric_value(metrics, calibrated, "lift_at_500")
    captured_at_500 = model_metric_value(metrics, calibrated, "readmissions_captured_at_500")
    net_value = MODEL_SUMMARY.get(
        "calibrated_expected_net_value_at_capacity",
        metrics.get("expected_net_value_at_capacity", 0),
    )
    captured_detail = (
        f"{int(captured_at_500):,} of 500 were readmitted"
        if not pd.isna(captured_at_500)
        else "selected patients readmitted"
    )
    cards = [
        metric_card("Precision@500", pct(precision_at_500), captured_detail),
        metric_card("Recall@500", pct(recall_at_500), "captures a focused high-risk slice"),
        metric_card("Lift@500", f"{lift_at_500:.2f}x" if lift_at_500 else "n/a", "better than random selection"),
        metric_card("Net Value", money(net_value), "default scenario"),
    ]
    net_value_direction = "positive" if net_value >= 0 else "negative"
    return f"""
    <div class="guidance-card model-summary">
        <div class="result-label">Model</div>
        <h2>{html.escape(MODEL_SUMMARY.get("model_name", "Readmission model"))}</h2>
        <p>Use calibrated risk to rank patients. Optimize the worklist, not accuracy.</p>
    </div>
    <div class="metric-grid">{"".join(cards)}</div>
    <div class="guidance-card">
        <div class="result-label">Interpretation</div>
        <ul>
            <li><strong>Precision@500:</strong> about {html.escape(pct(precision_at_500))} of selected patients were readmitted within 30 days.</li>
            <li><strong>Recall@500:</strong> the top 500 captures about {html.escape(pct(recall_at_500))} of all 30-day readmissions in the test set.</li>
            <li><strong>Lift@500:</strong> the worklist is about {lift_at_500:.2f}x richer in readmissions than random selection.</li>
            <li><strong>Net value:</strong> {html.escape(net_value_direction)} under the default cost and effectiveness assumptions.</li>
        </ul>
    </div>
    """
 
 
def limitations_html():
    items = "".join(f"<li>{item}</li>" for item in MODEL_SUMMARY.get("limitations", [])[:4])
    return f"""
    <div class="guidance-card">
        <div class="result-label">Use</div>
        <ul>{items}</ul>
    </div>
    """


def model_comparison_html():
    comparison_paths = [
        FINE_TUNED_COMPARISON_PATH,
        BASE_DIR / "artifacts" / "fine_tuned_model_comparison.csv",
        BASE_DIR.parent / "app" / "artifacts" / "fine_tuned_model_comparison.csv",
        BASE_DIR.parent / "artifacts" / "fine_tuned_model_comparison.csv",
    ]
    comparison = pd.DataFrame()
    for path in comparison_paths:
        if path.exists():
            comparison = pd.read_csv(path)
            break

    if comparison.empty:
        comparison = pd.DataFrame(MODEL_SUMMARY.get("fine_tuned_comparison", []))

    if comparison.empty:
        metrics = MODEL_SUMMARY.get("metrics", {})
        calibrated = MODEL_SUMMARY.get("calibrated_capacity_metrics", {})
        comparison = pd.DataFrame([
            {
                "model": f"Tabular {MODEL_SUMMARY.get('model_name', 'model')} (calibrated)",
                "avg_precision_pr_auc": metrics.get("avg_precision_pr_auc", np.nan),
                "precision_at_500": calibrated.get("precision_at_500", metrics.get("precision_at_500", np.nan)),
                "recall_at_500": calibrated.get("recall_at_500", metrics.get("recall_at_500", np.nan)),
                "lift_at_500": calibrated.get("lift_at_500", metrics.get("lift_at_500", np.nan)),
                "expected_net_value_at_capacity": MODEL_SUMMARY.get(
                    "calibrated_expected_net_value_at_capacity",
                    metrics.get("expected_net_value_at_capacity", np.nan),
                ),
            },
            {
                "model": "Fine-tuned prompt classifier",
                "avg_precision_pr_auc": np.nan,
                "precision_at_500": np.nan,
                "recall_at_500": np.nan,
                "lift_at_500": np.nan,
                "expected_net_value_at_capacity": np.nan,
            },
        ])

    if comparison.empty:
        return """
        <div class="guidance-card">
            <div class="result-label">Comparison</div>
            <p>Comparison loading.</p>
        </div>
        """

    cols = [
        "model",
        "avg_precision_pr_auc",
        "precision_at_500",
        "recall_at_500",
        "lift_at_500",
        "expected_net_value_at_capacity",
    ]
    comparison = comparison[[col for col in cols if col in comparison.columns]].copy()
    comparison = comparison.rename(
        columns={
            "model": "Model",
            "avg_precision_pr_auc": "PR-AUC",
            "precision_at_500": "Precision@500",
            "recall_at_500": "Recall@500",
            "lift_at_500": "Lift@500",
            "expected_net_value_at_capacity": "Net Value",
        }
    )

    for col in ["PR-AUC", "Precision@500", "Recall@500"]:
        if col in comparison:
            comparison[col] = comparison[col].map(lambda value: "n/a" if pd.isna(value) else f"{value:.3f}")
    if "Lift@500" in comparison:
        comparison["Lift@500"] = comparison["Lift@500"].map(lambda value: "n/a" if pd.isna(value) else f"{value:.2f}x")
    if "Net Value" in comparison:
        comparison["Net Value"] = comparison["Net Value"].map(lambda value: "n/a" if pd.isna(value) else money(value))

    table = comparison.to_html(index=False, classes="comparison-table", escape=False)
    fine_tuning_status = MODEL_SUMMARY.get(
        "fine_tuning_evaluation_status",
        "fine-tuned classifier comparison loaded from artifacts when available",
    )
    return f"""
    <div class="guidance-card">
        <div class="result-label">Comparison</div>
        {table}
        <p class="small-note">{html.escape(fine_tuning_status)} Decision: keep Gradient Boosting as the triage engine because it performs better for the structured triage decision.</p>
    </div>
    """
 
 
CUSTOM_CSS = f"""
@import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;800&display=swap');
.gradio-container {{
    max-width: 1120px !important;
    margin: 0 auto !important;
    padding: 20px !important;
    background: {SMU_LIGHT};
    color: {SMU_TEXT};
    font-family: "Google Sans", "Product Sans", Roboto, Arial, sans-serif !important;
}}
.gradio-container * {{
    font-family: "Google Sans", "Product Sans", Roboto, Arial, sans-serif !important;
    letter-spacing: 0 !important;
}}
#header-card {{
    background: white;
    border: 1px solid {SMU_BORDER};
    border-top: 4px solid {SMU_BLUE};
    border-radius: 8px;
    padding: 20px 22px;
    margin-bottom: 14px;
    box-shadow: 0 8px 22px rgba(31, 42, 68, 0.08);
}}
#header-card h1 {{
    color: {SMU_NAVY};
    margin: 0 0 4px 0;
    font-size: 1.65rem;
    font-weight: 800;
    letter-spacing: 0;
}}
#header-card p {{
    color: #5F6368;
    margin: 0;
    max-width: 850px;
    font-size: 0.98rem;
}}
.input-panel,
.output-panel,
.dashboard-panel {{
    background: white;
    border: 1px solid {SMU_BORDER};
    border-radius: 8px;
    padding: 18px;
    box-shadow: 0 6px 18px rgba(31, 42, 68, 0.06);
}}
.section-title {{
    color: {SMU_NAVY};
    font-size: 0.98rem;
    font-weight: 800;
    margin-bottom: 10px;
}}
.result-card {{
    border-radius: 8px;
    padding: 22px;
    min-height: 180px;
    text-align: center;
    display: flex;
    flex-direction: column;
    justify-content: center;
    background: white;
    border: 1px solid {SMU_BORDER};
}}
.high-result {{
    border-left: 4px solid {SMU_RED};
    background: rgba(204, 0, 53, 0.06);
}}
.safe-result {{
    border-left: 4px solid {SMU_BLUE};
    background: rgba(53, 76, 161, 0.06);
}}
.value-positive {{
    border-left: 4px solid #188038;
    background: rgba(46, 125, 50, 0.07);
}}
.value-negative {{
    border-left: 4px solid {SMU_RED};
    background: rgba(204, 0, 53, 0.06);
}}
.result-label {{
    color: {SMU_NAVY};
    font-size: 0.78rem;
    font-weight: 800;
    text-transform: uppercase;
}}
.result-value {{
    color: {SMU_NAVY};
    font-size: 2.35rem;
    font-weight: 800;
    margin: 10px 0 6px;
}}
.risk-value {{
    color: {SMU_RED};
    font-size: 1.08rem;
    font-weight: 800;
}}
.value-positive .risk-value,
.safe-result .risk-value {{
    color: {SMU_BLUE};
}}
.score-value {{
    color: #5F6368;
    margin-top: 10px;
    font-weight: 500;
}}
.metric-grid {{
    display: grid;
    grid-template-columns: repeat(4, minmax(0, 1fr));
    gap: 12px;
    margin: 12px 0;
}}
.metric-card {{
    border: 1px solid {SMU_BORDER};
    border-radius: 8px;
    padding: 16px;
    background: white;
}}
.metric-label {{
    color: {SMU_NAVY};
    font-size: 0.75rem;
    font-weight: 800;
    text-transform: uppercase;
}}
.metric-value {{
    color: {SMU_BLUE};
    font-size: 1.7rem;
    font-weight: 800;
    margin-top: 6px;
}}
.metric-detail,
.small-note {{
    color: #5F6368;
    font-size: 0.9rem;
}}
.guidance-card {{
    background: white;
    border: 1px solid {SMU_BORDER};
    border-radius: 8px;
    padding: 18px;
    box-shadow: 0 6px 18px rgba(31, 42, 68, 0.06);
}}
.guidance-card h2 {{
    margin: 6px 0 4px;
    color: {SMU_NAVY};
    font-size: 1.18rem;
    line-height: 1.3;
}}
.guidance-card p,
.guidance-card li {{
    color: {SMU_TEXT};
    line-height: 1.45;
}}
.guidance-section {{
    margin-top: 16px;
}}
.section-kicker {{
    color: {SMU_NAVY};
    font-size: 0.76rem;
    font-weight: 800;
    text-transform: uppercase;
    margin-bottom: 8px;
}}
.answer-box {{
    background: #F8FAFD;
    border: 1px solid #E8EEF7;
    border-left: 4px solid {SMU_BLUE};
    border-radius: 8px;
    padding: 14px 16px;
    color: #3C4043;
    line-height: 1.5;
}}
.action-list {{
    margin: 0;
    padding: 0;
    list-style: none;
}}
.action-list li {{
    margin: 8px 0;
    padding: 12px 14px;
    background: #F8FAFD;
    border: 1px solid #E8EEF7;
    border-left: 4px solid {SMU_BLUE};
    border-radius: 8px;
}}
.source-grid {{
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 12px;
}}
.source-card {{
    border: 1px solid #E8EEF7;
    border-radius: 8px;
    padding: 12px;
    background: #FFFFFF;
}}
.source-title {{
    color: {SMU_NAVY};
    font-weight: 800;
    font-size: 0.95rem;
    line-height: 1.25;
}}
.source-page {{
    color: #5F6368;
    font-size: 0.84rem;
    margin-top: 2px;
}}
.guidance-note {{
    margin-top: 14px;
    color: #5F6368;
    font-size: 0.86rem;
}}
.comparison-table {{
    width: 100%;
    border-collapse: collapse;
    margin-top: 10px;
    font-size: 0.92rem;
}}
.comparison-table th {{
    color: {SMU_NAVY};
    text-align: left;
    border-bottom: 1px solid {SMU_BORDER};
    padding: 10px 8px;
    font-weight: 800;
}}
.comparison-table td {{
    border-bottom: 1px solid #EEF2F7;
    padding: 10px 8px;
    color: {SMU_TEXT};
}}
.source-snippet {{
    margin-top: 8px;
    color: #3C4043;
    font-size: 0.88rem;
    line-height: 1.45;
}}
#run-score,
#run-scenario,
#run-guidance {{
    background: {SMU_BLUE} !important;
    border: none !important;
    color: white !important;
    font-weight: 800 !important;
    min-height: 42px !important;
    border-radius: 8px !important;
}}
#run-score:hover,
#run-scenario:hover,
#run-guidance:hover {{
    background: {SMU_NAVY} !important;
}}
footer {{
    display: none !important;
}}
button,
input,
textarea,
select,
.wrap {{
    border-radius: 8px !important;
}}
@media (max-width: 760px) {{
    .metric-grid {{
        grid-template-columns: repeat(2, minmax(0, 1fr));
    }}
    .source-grid {{
        grid-template-columns: 1fr;
    }}
    #header-card h1 {{
        font-size: 1.35rem;
    }}
}}
"""
 
 
with gr.Blocks(
    theme=gr.themes.Soft(primary_hue="blue", neutral_hue="slate"),
    css=CUSTOM_CSS,
    title="Triage Studio",
) as demo:
    gr.HTML(
        """
        <div id="header-card">
            <h1>Diabetes Readmission Triage</h1>
            <p>Rank patients. Plan capacity. Estimate value.</p>
        </div>
        """
    )
 
    shared_file = gr.File(
        label="Patient CSV",
        file_types=[".csv"],
        type="filepath",
    )
 
    with gr.Tab("Score"):
        with gr.Row(equal_height=True):
            with gr.Column(elem_classes="input-panel"):
                gr.HTML('<div class="section-title">Settings</div>')
                score_threshold = gr.Slider(
                    minimum=0.05,
                    maximum=0.75,
                    step=0.01,
                    value=DEFAULT_THRESHOLD,
                    label="High-risk threshold",
                )
                top_n = gr.Slider(
                    minimum=10,
                    maximum=100,
                    step=5,
                    value=25,
                    label="Rows shown",
                )
                score_button = gr.Button(
                    "Score",
                    variant="primary",
                    elem_id="run-score",
                )
 
            with gr.Column(elem_classes="output-panel"):
                gr.HTML('<div class="section-title">Summary</div>')
                score_summary = gr.HTML(
                    """
                    <div class="result-card">
                        <div class="result-label">Selected</div>
                        <div class="result-value">—</div>
                        <div class="score-value">Upload or use sample.</div>
                    </div>
                    """
                )
 
        score_table = gr.Dataframe(
            label="Worklist",
            wrap=True,
            interactive=False,
        )
 
    with gr.Tab("Plan"):
        with gr.Row(equal_height=True):
            with gr.Column(elem_classes="input-panel"):
                gr.HTML('<div class="section-title">Assumptions</div>')
                scenario_threshold = gr.Slider(
                    minimum=0.05,
                    maximum=0.75,
                    step=0.01,
                    value=DEFAULT_THRESHOLD,
                    label="Risk threshold",
                )
                selection_mode = gr.Radio(
                    choices=["Top N highest-risk patients", "All patients above threshold"],
                    value="Top N highest-risk patients",
                    label="Selection",
                )
                target_patients = gr.Slider(
                    minimum=25,
                    maximum=2000,
                    step=25,
                    value=DEFAULT_CAPACITY,
                    label="Target patients",
                )
                capacity = gr.Slider(
                    minimum=0,
                    maximum=2000,
                    step=25,
                    value=DEFAULT_CAPACITY,
                    label="Capacity cap",
                )
                readmission_cost = gr.Number(
                    value=DEFAULT_READMISSION_COST,
                    label="Readmission cost",
                    minimum=0,
                )
                intervention_cost = gr.Number(
                    value=DEFAULT_INTERVENTION_COST,
                    label="Intervention cost",
                    minimum=0,
                )
                effectiveness = gr.Slider(
                    minimum=0.01,
                    maximum=0.50,
                    step=0.01,
                    value=DEFAULT_EFFECTIVENESS,
                    label="Effectiveness",
                )
                scenario_button = gr.Button(
                    "Estimate",
                    variant="primary",
                    elem_id="run-scenario",
                )
 
            with gr.Column(elem_classes="output-panel"):
                gr.HTML('<div class="section-title">Value</div>')
                scenario_summary = gr.HTML(
                    """
                    <div class="result-card">
                        <div class="result-label">Net Value</div>
                        <div class="result-value">—</div>
                        <div class="score-value">Set assumptions.</div>
                    </div>
                    """
                )
 
        scenario_table = gr.Dataframe(
            label="Details",
            wrap=True,
            interactive=False,
        )
 
    with gr.Tab("Guidance"):
        with gr.Row(equal_height=True):
            with gr.Column(elem_classes="input-panel"):
                gr.HTML('<div class="section-title">Question</div>')
                gr.HTML('<div class="small-note">Ask about follow-up, medication review, appointments, or readmission cost.</div>')
                question = gr.Textbox(
                    label="Question",
                    value="What support should high-risk diabetes discharges receive?",
                    placeholder="Example: What follow-up actions should high-risk patients receive?",
                    lines=3,
                )
                openai_key = gr.Textbox(
                    label="OpenAI key",
                    type="password",
                    placeholder="sk-...",
                    info="Optional. Blank uses retrieval summary.",
                )
                guidance_button = gr.Button(
                    "Retrieve",
                    variant="primary",
                    elem_id="run-guidance",
                )
            with gr.Column(elem_classes="output-panel"):
                gr.HTML('<div class="section-title">Answer</div>')
                guidance_output = gr.HTML(
                    """
                    <div class="guidance-card">
                        <div class="result-label">Guidance</div>
                        <h2>Ready</h2>
                        <p class="small-note">Ask a care-transition question, then retrieve source-grounded guidance.</p>
                    </div>
                    """
                )
 
    with gr.Tab("Model"):
        gr.HTML(model_metrics_html())
        gr.HTML(model_comparison_html())
        gr.HTML(limitations_html())
 
    score_button.click(
        fn=score_patients,
        inputs=[shared_file, score_threshold, top_n],
        outputs=[score_summary, score_table],
    )
    scenario_button.click(
        fn=scenario_planner,
        inputs=[
            shared_file,
            scenario_threshold,
            capacity,
            selection_mode,
            target_patients,
            readmission_cost,
            intervention_cost,
            effectiveness,
        ],
        outputs=[scenario_summary, scenario_table],
    )
    guidance_button.click(
        fn=guidance_answer,
        inputs=[question, openai_key],
        outputs=guidance_output,
    )
 
 
demo.queue()
 
if __name__ == "__main__":
    demo.launch(ssr_mode=False)
 
