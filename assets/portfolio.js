const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const revealEls = document.querySelectorAll('.reveal');

  if (reduceMotion || !('IntersectionObserver' in window)) {
    revealEls.forEach(el => el.classList.add('visible'));
  } else {
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          entry.target.classList.add('visible');
          observer.unobserve(entry.target);
        }
      });
    }, { threshold: 0.08, rootMargin: '0px 0px -40px 0px' });

    revealEls.forEach((el, i) => {
      el.style.transitionDelay = `${i * 0.06}s`;
      observer.observe(el);
    });

    // Safety net: if the observer never fires (script error elsewhere,
    // unusual layout), don't leave content permanently invisible.
    setTimeout(() => revealEls.forEach(el => el.classList.add('visible')), 3000);
  }

  // Highlight the sidebar nav link matching whichever section is in view
  const navLinks = document.querySelectorAll('.side-nav a');
  const navSections = [...navLinks].map(a => document.querySelector(a.getAttribute('href')));

  // Use section positions so tall sections and upward scrolling stay in sync.
  let navFrame;
  function updateNavigation() {
    const current = [...navSections].reverse().find(section => section.getBoundingClientRect().top <= 140) || navSections[0];
    navLinks.forEach(link => {
      const active = link.hash === '#' + current.id;
      link.classList.toggle('active', active);
      if (active) link.setAttribute('aria-current', 'location');
      else link.removeAttribute('aria-current');
    });
    navFrame = null;
  }
  window.addEventListener('scroll', () => {
    if (!navFrame) navFrame = requestAnimationFrame(updateNavigation);
  }, { passive: true });
  window.addEventListener('resize', updateNavigation);
  updateNavigation();

  const projectSearch = document.getElementById('projectSearch');
  const projectCards = [...document.querySelectorAll('[data-project-card]')];
  const projectFilters = [...document.querySelectorAll('.project-filter[data-filter]')];
  const projectEmpty = document.getElementById('projectEmpty');
  const searchState = new URLSearchParams(location.search);
  let activeProjectFilter = projectFilters.some(button => button.dataset.filter === searchState.get('filter')) ? searchState.get('filter') : 'all';
  projectSearch.value = searchState.get('q') || '';
  document.querySelector('.search-shortcut').addEventListener('click', () => {
    projectSearch.focus({ preventScroll: true });
  });
  document.getElementById('resetProjects').addEventListener('click', () => {
    projectSearch.value = '';
    activeProjectFilter = 'all';
    updateProjectCards();
    projectSearch.focus({ preventScroll: true });
  });

  function normalize(value) {
    return value.toLowerCase().replace(/\s+/g, ' ').trim();
  }

  function updateProjectCards() {
    const query = normalize(projectSearch?.value || '');
    projectFilters.forEach(button => {
      const active = button.dataset.filter === activeProjectFilter;
      button.classList.toggle('active', active);
      button.setAttribute('aria-pressed', String(active));
    });
    const url = new URL(location.href);
    if (query) url.searchParams.set('q', projectSearch.value); else url.searchParams.delete('q');
    if (activeProjectFilter !== 'all') url.searchParams.set('filter', activeProjectFilter); else url.searchParams.delete('filter');
    if (url.href !== location.href) history.replaceState(null, '', url);

    let visibleCount = 0;

    projectCards.forEach(card => {
      const categories = normalize(card.dataset.categories || '').split(' ').filter(Boolean);
      const haystack = normalize(`${card.textContent} ${card.dataset.tags || ''}`);
      const matchesFilter = activeProjectFilter === 'all' || categories.includes(activeProjectFilter);
      const matchesQuery = query.split(' ').every(term => haystack.includes(term));
      const visible = matchesFilter && matchesQuery;
      card.hidden = !visible;
      card.classList.toggle('is-hidden', !visible);
      if (visible) visibleCount += 1;
    });

    if (projectEmpty) projectEmpty.hidden = visibleCount !== 0;
    document.getElementById('projectResults').textContent = `${visibleCount} of ${projectCards.length} projects and collections`;
  }

  projectSearch?.addEventListener('input', updateProjectCards);
  projectFilters.forEach(button => {
    button.addEventListener('click', () => {
      activeProjectFilter = button.dataset.filter;
      projectFilters.forEach(item => {
        item.classList.toggle('active', item === button);
        item.setAttribute('aria-pressed', item === button ? 'true' : 'false');
      });
      updateProjectCards();
    });
    button.setAttribute('aria-pressed', button.classList.contains('active') ? 'true' : 'false');
  });
  updateProjectCards();
