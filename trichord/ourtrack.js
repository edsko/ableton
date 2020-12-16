/**
 * Custom Push2 instrument: Trichords
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 *
 * This code is intended as a tutorial, not for production usage.
 *
 * @module ourtrack
 * Interface to the track on which the M4L device is loaded.
 */

/*******************************************************************************
  Public API
*******************************************************************************/

/**
 * Constructor
 *
 * Should not be called until the M4L device is fully initialized.
 *
 * @param callback Function called whenever the selected track changes
 *                 Gets passed an argument telling it if our track was selected.
 */
exports.OurTrack = function(callback) {
  var track    = new LiveAPI(null, "this_device canonical_parent");

  // Register a callback to be run every time the selected track changes
  // We make it part of 'this' so that its lifetime is tied to ours;
  // if we didn't, the callback would not be called anymore the moment we leave
  // the scope of this constructor).
  outerThis = this;
  this.view = new LiveAPI(function(args) {
    if(args[0] == "selected_track") {
      if (track.id == args[2]) {
        outerThis.selected = true;
        callback(true);
      } else {
        outerThis.selected = false;
        callback(false);
      }
    }
  });
  this.view.path     = "live_set view";
  this.view.property = "selected_track";
}

/**
 * Class
 */
exports.OurTrack.prototype = {
  /**
   * Report if our track is currently selected
   */
  isSelected: function() {
    return this.selected;
  }
};
