/**
 * Custom Push2 instrument: Trichords
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 *
 * This code is intended as a tutorial, not for production usage.
 *
 * @module buttonmatrix
 * Interface to the button matrix of the Push2 controller.
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Constructor
 *
 * @param push     Reference to the Push2 controller
 * @param callback Called when a button is pressed or released
 */
exports.ButtonMatrix = function(push, callback) {
  this.buttonMatrix = findButtonMatrix(push, function(args) {
    if(args[0] == "value" && args.length == 5) {
      callback({"velocity": args[1], "col": args[2], "row": args[3]});
    }
  });
}

/**
 * Class
 */
exports.ButtonMatrix.prototype = {
  /**
   * Set the color of one of the buttons
   */
  setColor: function(col, row, color) {
    this.buttonMatrix.call("send_value", col, row, color);
  }

  /**
   * Delete all callbacks.
   *
   * This will cause the ButtonMatrix to stop listening for key presses
   * Should should be called before the button matrix falls out of scope.
   */
, deleteCallbacks: function() {
    this.buttonMatrix.property = "";
  }
}

/*******************************************************************************
  Private functions
*******************************************************************************/

/**
 * Find the button matrix on the Push2 controller
 */
function findButtonMatrix(push, callback) {
  var buttonMatrixId = push.call("get_control", "Button_Matrix");
  var buttonMatrix   = new LiveAPI(callback, buttonMatrixId);
  buttonMatrix.property = "value"; // Monitor the buttons
  return buttonMatrix;
}
