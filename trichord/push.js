/**
 * Custom Push2 instrument: Trichords
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 *
 * This code is intended as a tutorial, not for production usage.
 *
 * @module push
 * Interface to the Push2 controller.
 */

/*******************************************************************************
  Imports
*******************************************************************************/

var ButtonMatrix = require("buttonmatrix").ButtonMatrix;
var Grid         = require("grid").Grid;

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Constructor
 *
 * Should not be called until the M4L device is fully initialized.
 *
 * @param object Object to call the actions on
 */
exports.Push = function(object) {
  this.actionObject = object;
  this.controller   = findPush();
  this.colorGrid    = new Grid(8, 8, 0);
  this.actionGrid   = new Grid(8, 8, null);
  this.buttonMatrix = new ButtonMatrix(this.controller, this, buttonPressed);
};

/**
 * Class
 */
exports.Push.prototype = {
  /**
   * Return if the Push2 controller was found
   */
  checkFound: function() {
    return (this.controller != null);
  }

  /**
   * Set control of the button matrix
   *
   * @param control 'true' if we want to control the button matrix
   */
, controlButtonMatrix: function(control) {
    if(!this.checkFound()) return;

    if(control) {
      this.controller.call("grab_control", "Button_Matrix");

      // We initialize the colors after a short delay. If we initialize them
      // right here, switching between tracks works just fine within Ableton
      // itself, but for some reason it does not work if we switch track using
      // the buttons on the Push.
      var initColorsTask = new Task(initColors, this);
      initColorsTask.schedule(0);
    } else {
      this.controller.call("release_control", "Button_Matrix");
    }
  }

  /**
   * Set the color of one of the buttons
   */
, setColor: function(col, row, color) {
    if(!this.checkFound()) return;
    this.colorGrid.set(col, row, color);
    this.buttonMatrix.setColor(col, row, color);
  }

  /**
   * Set action for one of the buttons
   */
, setAction: function(col, row, callback) {
    this.actionGrid.set(col, row, callback);
  }

  /**
   * Show all possible colors
   *
   * @param page Which page of colors (0 or 1)
   */
, showColors: function(page) {
    this.colorGrid.fill(page * 64, 1);
    initColors.call(this);
  }

  /**
   * Delete all callbacks.
   */
, deleteObservers: function() {
    this.buttonMatrix.deleteObservers();
  }
};

/*******************************************************************************
  Private functions
*******************************************************************************/

/**
 * Find the Push2 controller
 *
 * NOTE: We do not take into account that there might be more than one.
 */
function findPush() {
  var liveApp            = new LiveAPI(null, "live_app")
  var numControlSurfaces = liveApp.getcount("control_surfaces");

  for (var i = 0; i < numControlSurfaces; i++) {
    var controlSurface = new LiveAPI(null, "control_surfaces " + i);
    if (controlSurface.type == "Push2") {
      return controlSurface;
    }
  }

  return null;
}

/**
 * Initialize the colors of the button matrix after grabbing control
 */
function initColors() {
  this.colorGrid.traverse(this.buttonMatrix, this.buttonMatrix.setColor);
}

/**
 * Handle button presses
 */
function buttonPressed(col, row, velocity) {
  var action = this.actionGrid.get(col, row);
  var color  = this.colorGrid.get(col, row);
  if (action != null) {
    action.call(this.actionObject, col, row, color, velocity);
  }
}
