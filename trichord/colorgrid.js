/**
 * Custom Push2 instrument: Trichords
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 *
 * This code is intended as a tutorial, not for production usage.
 *
 * @module colorgrid
 * Simple 2D color grid
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Constructor
 *
 * @param cols Number of columns
 * @param rows Number of rows
 */
exports.ColorGrid = function(cols, rows) {
  this.grid = new Array();

  for(var i = 0; i < cols; i++) {
    var col = new Array();

    for(var j = 0; j < rows; j++) {
      col[j] = 0;
    }

    this.grid[i] = col;
  }
}

/**
 * Class
 */
exports.ColorGrid.prototype = {
  /**
   * Set a color in the grid
   */
  setColor: function(col, row, color) {
    this.grid[col][row] = color;
  }

  /**
   * Call the callback for each entry in the grid
   */
, traverse: function(object, callback) {
    for(var i in this.grid) {
      var col = this.grid[i];

      for(var j in col) {
        callback.call(object, i, j, col[j]);
      }
    }
  }

  /**
   * Initialize each entry in the grid the callback
   *
   * The call back is passed the 'col' and 'row' parameters
   */
, init: function(callback) {
    for(var i in this.grid) {
      var col = this.grid[i];

      for(var j in col) {
        col[j] = callback(i, j);
      }
    }
  }

  /**
   * Initialize the grid
   *
   * @param from Starting value
   */
, fill: function(from) {
    this.init(function(col, row) { return from++; });
  }
}
