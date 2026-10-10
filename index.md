
---
layout: default
title: LeafInjuryR
---

<script>
window.MathJax = {
  tex: {
    inlineMath: [['\\(', '\\)']],
    displayMath: [['$$', '$$'], ['\\[', '\\]']],
    processEscapes: true
  },
  svg: {
    fontCache: 'global'
  }
};
</script>

<script defer
  src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js">
</script>

<style>
/* Preserve image responsiveness */
.leafinjuryr-content img {
  max-width: 100%;
  height: auto;
}

/* Keep badges properly aligned */
.leafinjuryr-content a img {
  display: inline-block;
  vertical-align: middle;
}

/* Center elements using align=center */
.leafinjuryr-content [align="center"] {
  text-align: center;
}

/* Prevent image overflow */
.leafinjuryr-content {
  overflow-wrap: break-word;
}

/* Improve academic profile SVG display */
.leafinjuryr-content img[src$="academic-profile.svg"] {
  display: block;
  max-width: 100%;
  height: auto;
  margin: 16px auto;
}

/* Keep contact badges aligned */
.leafinjuryr-content a {
  text-decoration: none;
}

/* Improve responsive layout */
@media (max-width: 768px) {
  .leafinjuryr-content img {
    max-width: 100%;
    height: auto;
  }

  .leafinjuryr-content table {
    display: block;
    overflow-x: auto;
  }
}
</style>

{% capture readme_content %}{% include_relative README.md %}{% endcapture %}

{% assign readme_clean = readme_content %}

{% assign readme_first_char = readme_clean | strip | slice: 0, 3 %}

{% if readme_first_char == '---' %}
  {% assign readme_parts = readme_clean | split: '---' %}
  {% assign readme_clean = readme_parts | shift | shift | join: '---' %}
{% endif %}

<div class="leafinjuryr-content">
{{ readme_clean | markdownify }}
</div>
