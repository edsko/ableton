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
var ColorGrid    = require("colorgrid").ColorGrid;

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Constructor
 *
 * @param buttonPressed Callback invoked when a button is pressed or released
 */
exports.Push = function(buttonPressed) {
  // We cannot locate the Push until the Max device is fully initialized
  this.controller    = null;
  this.buttonPressed = buttonPressed;
  this.colorGrid     = new ColorGrid(8, 8);
};

/**
 * Class
 */
exports.Push.prototype = {
  /**
   * Locate the Push controller
   *
   * Should be called when the Max live is fully loaded; use 'live.thisdevice'.
   */
  init: function() {
    if(this.controller != null) return;
    this.controller = findPush();
    if(this.controller == null) return;

    var outerThis = this;
    this.buttonMatrix = new ButtonMatrix(this.controller, this.buttonPressed);
  }

  /**
   * Return if the Push2 controller was found
   */
, checkFound: function() {
    return (this.controller != null);
  }

  /**
   * Grab control of the button matrix
   */
, grab: function() {
    if(!this.checkFound()) return;
    this.controller.call("grab_control", "Button_Matrix");

    var outerThis = this;
    this.colorGrid.traverse(function(col, row, color) {
      outerThis.buttonMatrix.setColor(col, row, color);
    });
  }

  /**
   * Release control of the button matrix
   */
, release: function() {
    if(!this.checkFound()) return;
    this.controller.call("release_control", "Button_Matrix");
  }

  /**
   * Set the color of one of the buttons
   */
, setColor: function(col, row, color) {
    if(!this.checkFound()) return;
    this.colorGrid.setColor(col, row, color);
    this.buttonMatrix.setColor(col, row, color);
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
