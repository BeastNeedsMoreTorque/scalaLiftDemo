"use strict";
// Global variable, singleton lcboViewer
var lcboViewer = (function() {
    var currPosition = null;
    return {
      fetchStoreFromPosition: function() {
          if (navigator.geolocation) {
              navigator.geolocation.getCurrentPosition(lcboViewer.showPosition);
          } else  {
              $("#storeNearby").html("Geolocation is not supported by this browser.");
          }
        },

      showPosition: function(position) {
          // Show coordinates now, later on a map centered at position
          if (typeof position != 'undefined') currPosition = position.coords;
          $.ajax({
              url: '/store/lat/' + currPosition.latitude + '/lon/' +currPosition.longitude,
              type: 'GET',
              success: function(data, status){
                  var coords = position.coords;
                  var storeDesc = 'You are located at (' + coords.latitude.toPrecision(6) + ',' + coords.longitude.toPrecision(6) +
                      '), which is at ' + data.distance_in_meters/1000.0 + ' kms from branch '+ data.name  ;
                  $("#storeNearby").html(storeDesc);
                  },
              error: function(data, status){
                  console.log("Error Data: " + data + "\nStatus: " + status );
              }
          });
      }
  }
}());


