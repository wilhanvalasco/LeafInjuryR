---
layout: leafinjuryr
title: LeafInjuryR | Leaf Injury Quantification in R
description: LeafInjuryR, an open-source R package for automated leaf injury quantification using RGB images and computer vision.
---

{% capture readme %}{% include_relative README.md %}{% endcapture %}
{{ readme
   | replace: "[!NOTE]", "**Note.**"
   | replace: "[!IMPORTANT]", "**Important.**"
   | replace: "[!TIP]", "**Tip.**"
   | replace: "[!WARNING]", "**Warning.**" }}
