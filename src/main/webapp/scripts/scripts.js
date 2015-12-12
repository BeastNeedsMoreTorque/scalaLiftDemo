"use strict";
// Global variable, singleton LCBOviewer
var lcboViewer = (function() {

  return {
    showPosition: function(position) {
        // Show coordinates now, later on a map centered at position
        $("#geolocation").html('You are at Latitude:' + position.coords.latitude + ' Longitude:' + position.coords.longitude + ' where is the next LCBO?');
    },

    start: function () {
        if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(lcboViewer.showPosition);
        } else {
            $("#geolocation").html("Geolocation is not supported by this browser.");
        }
    }
  }
}());


