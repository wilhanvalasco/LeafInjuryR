
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
</style>

<div class="leafinjuryr-content" markdown="1">

{% capture readme %}{% include_relative README.md %}{% endcapture %}
{{ readme | markdownify }}

</div>
