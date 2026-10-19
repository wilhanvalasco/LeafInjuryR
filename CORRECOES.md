# LeafInjuryR 0.2.1 — ajustes de distribuição

- Instruções de instalação atualizadas para não atualizar indiscriminadamente bibliotecas.
- Versão e requisito mínimo do R alinhados entre DESCRIPTION e README.
- Excluídos do arquivo de distribuição: `.git`, `Data base`, `.Rproj.user`, `results`, arquivos de histórico e arquivos gerados no macOS.
- Adicionado `INSTALL_WINDOWS.R` para instalação guiada.
- A ocorrência de `dialog::` no relatório é CSS (`dialog::backdrop`), não uma dependência do pacote R `dialog`.

## Validação pendente

Não foi possível executar `R CMD check` nem instalação em Windows nesta sessão. Execute os testes em ambiente R limpo antes de publicar. Os algoritmos em `R/` não foram alterados.
