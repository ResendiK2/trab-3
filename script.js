const tabuleiro = document.getElementById("tabuleiro");

function renderTabuleiro() {
  tabuleiro.innerHTML = "";

  for (let i = 0; i < 8; i++) {
    for (let j = 0; j < 8; j++) {
      const casa = document.createElement("div");
      casa.classList.add("casa", (i + j) % 2 === 0 ? "clara" : "escura");
      tabuleiro.appendChild(casa);
    }
  }
}

renderTabuleiro();
