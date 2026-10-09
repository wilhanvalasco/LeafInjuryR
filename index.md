
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

<div class="leafinjuryr-content" markdown="1">

{% capture readme %}{% include_relative README.md %}{% endcapture %}
{{ readme | markdownify }}

</div>
