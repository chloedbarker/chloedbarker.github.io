# Diabetes Transitional Care Triage

This Gradio app supports a hospital care-management decision: which diabetes discharge
encounters should receive enhanced transitional-care support under limited staffing and
budget constraints.

The app includes:

- Readmission risk scoring and patient prioritization.
- Cost, capacity, and net-value scenario planning.
- A model comparison tab that includes the tuned/calibrated tabular classifier and the
  completed fine-tuned prompt classifier. The app keeps Gradient Boosting as the main
  triage engine because it performs better on the structured readmission-ranking decision.
- A RAG tab over the CMS/AHRQ/Project RED PDFs in `pdfs/`: PDF loading/chunking, dense
  embeddings (`sentence-transformers/all-MiniLM-L6-v2`) indexed with FAISS for vector
  search, an optional OpenAI-generated answer grounded in the retrieved sources, and source citations
  on every answer.
- Model metrics, limitations, and appropriate-use guidance.

Educational prototype only. Do not use for real clinical decisions.
