---
title: "Relatório de auditoria – Manuscrito LeafInjuryR (versão 6)"
date: "Outubro de 2026"
---

# 1. Alterações desta versão

**Versão 6:**

- Análises e figuras refeitas com o Rmd revisado pelo autor (Figura 4b só com mediana e intervalo interquartil; Figura 6 só com banda de confiança, eixo Y de 0 a 50 e formato largo).
- "Severidade visual" passou a se chamar **severidade observada (%)**, definida como a mediana dos quatro avaliadores, no texto, nas tabelas e nos eixos e títulos das figuras (PT e EN).
- A escala diagramática de Peixoto (2023) está descrita na metodologia: 12 níveis, construção a partir do Assess e validação com oito avaliadores (R² de 0,77 a 0,91).
- Figura 7 com títulos em uma linha ("LeafInjuryR x% · Observada y%").
- Versão PT com 16 páginas e versão EN com 15.

**Versão 4:**

- Figuras 2 e 7 usam as cores padrão do LeafInjuryR (verde: saudável; amarelo: clorótico; vermelho: necrótico; magenta: outro), iguais às do aplicativo. Os demais gráficos mantêm a paleta fria.
- A Figura 6a mostra apenas o modelo de potência (melhor por AICc e por validação cruzada), com a equação, o R², a banda de confiança e a banda de predição.
- A Tabela 4 (comparação qualitativa com ImageJ/Fiji, PlantCV, pliman e Assess/Leaf Doctor) foi reintroduzida.
- Versão PT com 15 páginas e versão EN com 14.

**Versão 3:**

- **Tratamentos removidos** de todas as análises, figuras e do texto; a coluna da planilha serve apenas para localizar os arquivos.
- **Novo foco:** quanto o LeafInjuryR prediz a avaliação visual (resposta = mediana visual; preditor = LeafInjuryR). Cinco modelos (linear, quadrático, potência, logarítmico e assintótico) comparados por AICc e validação cruzada *leave-one-out*, com bandas de confiança e de predição de 95% (analíticas nos lineares; bootstrap de casos, 2.000 reamostragens, nos não lineares).
- **Nova Figura 5:** variabilidade entre avaliadores e impacto nas decisões.
- **Contexto biológico:** soja inoculada com *Corynespora cassiicola* (mancha-alvo).
- **Paleta fria** em todos os gráficos e nas sobreposições das folhas (tecido saudável: verde-azulado; clorótico: azul-claro; necrótico: azul-marinho; outro: lilás).
- **Todas as figuras em PT e EN**, geradas pelo Rmd `analise_LeafInjuryR_artigo.Rmd`. A Figura 3 é captura de tela da interface (em português nas duas versões).
- **Figura 7:** sem tratamentos, apenas "Imagem 1–4", escolhidas por quantis de severidade (10%, 45%, 75% e 97%).
- **Extensão:** versão PT com 15 páginas e versão EN com 14, ambas dentro do limite de 20; 40 referências (somente as citadas).
- **Densidade de kernel:** não usada. Com n = 48, curvas de densidade seriam pouco informativas; os eixos foram ajustados ao intervalo dos dados.

# 2. Principais resultados

| Indicador | Valor |
|---|---|
| Amplitude entre avaliadores | mediana 4,5 p.p.; ≥ 10 p.p. em 16,7% dos folíolos; máximo 58,3 p.p. |
| CV mediano entre avaliadores | 21,9% |
| ICC(A,1) / ICC(A,4) | 0,73 (0,53–0,90) / 0,91 (0,82–0,97) |
| Folíolos com decisão divergente (limiares 5, 10, 15, 20, 25%) | 12,5; 12,5; 20,8; 25,0; 16,7% |
| Melhor modelo | potência: y = 2,264·x^0,637 (menor AICc e menor RMSE em validação cruzada) |
| LeafInjuryR bruto × mediana visual | CCC 0,703 (0,600–0,784); r 0,903; EAM 6,50 p.p. |
| LeafInjuryR calibrado (validação cruzada) | CCC 0,892 (0,818–0,938); EAM 2,77 p.p. |
| Avaliadores × mediana dos demais | CCC 0,632; 0,897; 0,927; 0,904 |

**Mensagem central:** depois de calibrado, o LeafInjuryR prediz o consenso visual com concordância equivalente à de avaliadores experientes, e de forma reprodutível. Sem calibração, superestima acima de 10%, provavelmente porque contabiliza o halo clorótico. Essa explicação é uma hipótese exploratória.

# 3. Cautelas

1. A calibração foi estimada e avaliada no mesmo conjunto. A validação cruzada *leave-one-out* reduz, mas não elimina, o otimismo; recomenda-se um conjunto independente de imagens.
2. A referência visual não é a verdade; não houve máscaras pixel a pixel.
3. Um dos avaliadores (A1) é autor do pacote; declare isso na seção de conflito de interesses da revista.
4. O texto afirma que a escala de Peixoto (2023) representa lesões em preto (verificado no PDF fornecido).
5. Não foram adicionadas referências específicas sobre a mancha-alvo, porque não foi possível verificar dados bibliográficos nesta sessão. Recomenda-se incluir 2–3 referências sobre *C. cassiicola* em soja.

# 4. Campos a completar no manuscrito

- Cultivar, local, safra, estádio fenológico, método de inoculação e câmera (marcados entre colchetes na seção 2.2).
- Afiliação, ORCID e coautores.
- Confirmação de que as avaliações visuais foram feitas às cegas e em ordem aleatória.
- Conferência dos DOIs das 40 referências (sem acesso a bases nesta sessão).
- DOI do Zenodo.

# 5. Revista-alvo

*Tropical Plant Pathology* continua sendo a opção mais alinhada, pelo foco em fitopatometria. Com um conjunto de validação independente ou máscaras pixel a pixel, *Plant Methods* torna-se viável.
