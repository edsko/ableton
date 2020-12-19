/**
 * Custom Push2 instrument: Trichords
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 *
 * This code is intended as a tutorial, not for production usage.
 *
 * @module grid
 * Simple 2D grid
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Constructor
 *
 * @param cols Number of columns
 * @param rows Number of rows
 * @param init Initial value for each cell
 */
exports.Grid = function(cols, rows, init) {
  this.grid = new Array();

  for(var i = 0; i < cols; i++) {
    var col = new Array();

    for(var j = 0; j < rows; j++) {
      col[j] = init;
    }

    this.grid[i] = col;
  }
}

/**
 * Class
 */
exports.Grid.prototype = {
  /**
   * Get the value of a cell in the grid
   */
  get: function(col, row) {
    return this.grid[col][row];
  }

  /**
   * Set a cell in the grid
   */
, set: function(col, row, value) {
    this.grid[col][row] = value;
  }

  /**
   * Call the callback for each cell in the grid
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
   * Initialize each cell in the grid using the callback
   *
   * The callback is passed the 'col' and 'row' parameters
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
