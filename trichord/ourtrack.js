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
exports.OurTrack = function(object, callback) {
  var track   = new LiveAPI(null, "this_device canonical_parent");
  var trackId = track.id;

  // Register a callback to be run every time the selected track changes
  // We make it part of 'this' so that its lifetime is tied to ours;
  // if we didn't, the callback would not be called anymore the moment we leave
  // the scope of this constructor).
  outerThis = this;
  this.view = new LiveAPI(function(args) {
    if(args[0] == "selected_track") {
      if (args[2] == trackId) {
        outerThis.selected = true;
        callback.call(object, true);
      } else {
        outerThis.selected = false;
        callback.call(object, false);
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

  /**
   * Delete all callbacks.
   *
   * This means we will stop watching the track.
   *
   * It is important to call this function before allowing the object to
   * fall out of scope, otherwise the view and its callback will continue
   * to keep each other alive.
   *
   * @see {@link https://cycling74.com/forums/how-to-destroy-a-liveapi-object-instantiated-in-js} for more information.
   */
, deleteCallbacks: function() {
    this.view.property = ""; // null does not work!
  }
};
