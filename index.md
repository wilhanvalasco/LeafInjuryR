
---
layout: default
title: LeafInjuryR
---

<script>
window.MathJax = {
  tex: {
    inlineMath: [['\\(', '\\)']],
    displayMath: [['$$', '$$'], ['\\[', '\\]']]
  },
  svg: {
    fontCache: 'global'
  }
};
</script>
<script defer src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"></script>

{% capture readme %}
{% include_relative README.md %}
{% endcapture %}

{{ readme | markdownify }}
