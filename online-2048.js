function keyDown(k) {
    var oEvent = document.createEvent('KeyboardEvent');

    // Chromium Hack
    Object.defineProperty(oEvent, 'keyCode', {
                get : function() {
                    return this.keyCodeVal;
                }
    });
    Object.defineProperty(oEvent, 'which', {
                get : function() {
                    return this.keyCodeVal;
                }
    });

    if (oEvent.initKeyboardEvent) {
        oEvent.initKeyboardEvent("keydown", true, true, document.defaultView, false, false, false, false, k, k);
    } else {
        oEvent.initKeyEvent("keydown", true, true, document.defaultView, false, false, false, false, k, 0);
    }

    oEvent.keyCodeVal = k;

    if (oEvent.keyCode !== k) {
        alert("keyCode mismatch " + oEvent.keyCode + "(" + oEvent.which + ")");
    }

    Object.defineProperty(oEvent, 'target', {
                get : function() {
                    return { tagName: 'stub' };
                }
    });

    Object.defineProperty(oEvent, 'metaKey', {
                get : function() {
                    return false;
                }
    });

    Object.defineProperty(oEvent, 'shiftKey', {
                get : function() {
                    return false;
                }
    });

    document.dispatchEvent(oEvent);
}

function playMove() {
  if (!window.localStorage.gameState)
    return false;

  const grid = JSON.parse(window.localStorage.gameState).grid;
  const gridCells = [];
  for (var x = 0; x < 4; x++) {
    const row = [];
    for (var y = 0; y < 4; y++) {
      const c = grid.cells[y][x];
      row.push(c && c.value);
    }

    gridCells.push(row);
  }
  keyDown(org.quach.Online2048().getMove(gridCells));

  return true;
}

window.playGame = function() {
  const i = setInterval(() => {
    const result = playMove();
    if (!result)
      clearInterval(i);
  }, 100);
}
