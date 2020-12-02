/**
 * Push animation support
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 */

/**
 * Create an empty frame
 *
 * Returns a 8x8 2D matrix.
 */
function mkEmptyFrame() {
  var frame = new Array();
  for(var i = 0; i < 8; i++) {
    var col = new Array();
    frame[i] = col;

    for(var j = 0; j < 8; j++) {
      col[j] = 0;
    }
  }

  return frame;
}

/**
 * Construct a frame from a series of instructions
 *
 * Each instruction should be a a list of 3 elements: row, column, color.
 */
function constructFrame(instructions) {
  var frame = mkEmptyFrame();

  for(var i in instructions) {
    var instruction = instructions[i];
    frame[instruction[0]][instruction[1]] = instruction[2];
  }

  return frame;
}

/**
 * The Animator class
 */
var animatorPrototype = {
  start: function() {
    this.index   = 0;
    this.current = mkEmptyFrame();
    this.task.repeat();
  }

, stop: function() {
    this.task.cancel();
  }

, tick: function() {
    this.update(constructFrame(this.animData[this.index]));
    this.index = (this.index + 1) % this.animData.length;
  }

  /**
   * Update the state of the push
   */
, update: function(newFrame) {
    for(var i = 0; i < 8; i++) {
      for(var j = 0; j < 8; j++) {
        var oldColor = this.current[i][j];
        var newColor = newFrame[i][j];
        if(oldColor != newColor) {
          this.buttonMatrix.call("send_value", i, j, newColor);
          this.current[i][j] = newColor;
        }
      }
    }
  }
};

function Animator(buttonMatrix, animData) {
  this.buttonMatrix = buttonMatrix;
  this.animData     = animData;
  this.task         = new Task(this.tick, this);

  // TODO: We should synchronize this to the clock.
  this.task.interval = 250;
}
Animator.prototype = animatorPrototype;
exports.Animator = Animator;
