// Minimal Inertia.js-compatible client for causal-proof-web.
// In production, replace with a full Inertia + Vue/React/Svelte setup.
// This vanilla JS version renders the server-provided page props directly.

(function() {
  'use strict';

  var appEl = document.getElementById('app');
  if (!appEl) return;

  var page;
  try {
    page = JSON.parse(appEl.getAttribute('data-page'));
  } catch(e) {
    appEl.innerHTML = '<p>Failed to parse page data.</p>';
    return;
  }

  // Component renderers
  var components = {
    Backends: function(props) {
      var html = '<h1>Causal Proof Backends</h1>';
      html += '<p>Unified proof assistant interface &mdash; ';
      html += '<a href="/api/health">health</a> | ';
      html += '<a href="/api/backends">JSON</a></p>';
      html += '<table border="1" cellpadding="8" cellspacing="0">';
      html += '<tr><th>Name</th><th>Mode</th><th>Ext</th><th>Operations</th></tr>';
      (props.backends || []).forEach(function(b) {
        html += '<tr>';
        html += '<td><strong>' + b.name + '</strong></td>';
        html += '<td><code>' + b.mode + '</code></td>';
        html += '<td>' + (b.ext || '') + '</td>';
        html += '<td>' + b.ops.join(', ') + '</td>';
        html += '</tr>';
      });
      html += '</table>';
      html += '<h2>Dispatch</h2>';
      html += '<pre>curl -X POST http://localhost:8420/api/dispatch \\\n';
      html += '  -H "Content-Type: application/json" \\\n';
      html += '  -d \'{"backend":"Narya","op":"step-fwd"}\'</pre>';
      html += '<h2>Related Files</h2>';
      html += '<ul>';
      html += '<li><code>causal-proof.el</code> &mdash; main Transient menu</li>';
      html += '<li><code>causal-proof-utils.el</code> &mdash; backend registry &amp; dispatch</li>';
      html += '<li><code>geb-mc.lisp</code> &mdash; Geb morphism MC sampler (ABCL)</li>';
      html += '<li><code>sophia-mnemosyne.el</code> &mdash; RDF knowledge graph</li>';
      html += '</ul>';
      return html;
    }
  };

  var render = components[page.component];
  if (render) {
    appEl.innerHTML = render(page.props);
  } else {
    appEl.innerHTML = '<p>Unknown component: ' + page.component + '</p>';
    appEl.innerHTML += '<pre>' + JSON.stringify(page, null, 2) + '</pre>';
  }

  // Intercept link clicks for Inertia-style navigation
  document.addEventListener('click', function(e) {
    var link = e.target.closest('a');
    if (!link || !link.href || link.getAttribute('target')) return;
    var url = new URL(link.href);
    if (url.origin !== location.origin) return;
    if (url.pathname.startsWith('/api/')) return; // let API links open normally
    e.preventDefault();
    fetch(url.pathname, {
      headers: { 'X-Inertia': 'true', 'X-Inertia-Version': page.version || '1' }
    })
    .then(function(r) { return r.json(); })
    .then(function(newPage) {
      page = newPage;
      var r2 = components[newPage.component];
      if (r2) appEl.innerHTML = r2(newPage.props);
      history.pushState({}, '', newPage.url);
    });
  });
})();
