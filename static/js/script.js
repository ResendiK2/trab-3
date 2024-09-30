const tabuleiro = [
  ["b", "j1", "b", "j1", "b", "j1", "b", "j1"],
  ["j1", "b", "j1", "b", "j1", "b", "j1", "b"],
  ["b", "j1", "b", "j1", "b", "j1", "b", "j1"],
  ["b", "b", "b", "b", "b", "b", "b", "b"],
  ["b", "b", "b", "b", "b", "b", "b", "b"],
  ["j2", "b", "j2", "b", "j2", "b", "j2", "b"],
  ["b", "j2", "b", "j2", "b", "j2", "b", "j2"],
  ["j2", "b", "j2", "b", "j2", "b", "j2", "b"],
];

function renderTabuleiro() {
  const tabuleiroElement = document.getElementById("tabuleiro");
  tabuleiroElement.innerHTML = ""; // Limpa o tabuleiro

  for (let i = 0; i < tabuleiro.length; i++) {
    for (let j = 0; j < tabuleiro[i].length; j++) {
      const casa = document.createElement("div");
      casa.classList.add("casa", (i + j) % 2 === 0 ? "clara" : "escura");

      const peca = tabuleiro[i][j];
      if (peca === "j1") {
        const pecaElemento = document.createElement("div");
        pecaElemento.classList.add("j1");
        casa.appendChild(pecaElemento);
      } else if (peca === "j2") {
        const pecaElemento = document.createElement("div");
        pecaElemento.classList.add("j2");
        casa.appendChild(pecaElemento);
      }

      tabuleiroElement.appendChild(casa);
    }
  }
}

renderTabuleiro();
