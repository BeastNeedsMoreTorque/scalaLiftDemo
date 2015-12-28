"use strict";
// Global variable, singleton lcboViewer
// API:
// <localhost:port>/store/lat/<lat>/lon/<lon> gives closest store from coordinate(lat,lon).
// Currently supported Store  (see lcboapi.com): id, name, is_dead, distance_in_meters (from specified location), address_line_1, city
var lcboViewer = (function() {
    var currPosition = null;
    var imgSelectStyle = 'imgframe';
    var fetchStore = function(position) {
        // Show coordinates and store now, later, on a map centered at position
        if (typeof position != 'undefined') currPosition = position.coords;
        $.ajax({
            url: '/store/lat/' + currPosition.latitude.toString().replace('.',  ',') + '/lon/' +currPosition.longitude.toString().replace('.', ',')+ '.json', // use commas to help liftweb Req parsing in REST
            type: 'GET',
            success: function(data, status){
                var coords = position.coords;
                // #storeNearby: You are XYZ.NN kms from branch <name> located at <address> in <city>
                var storeDesc = 'You are ' + (data.distance_in_meters/1000.0).toFixed(2) + ' kms from branch '+ data.name +
                    ' located at ' + data.address_line_1 + ' in ' + data.city;
                $("#storeNearby").html(storeDesc);
                },
            error: function(data, status){
                console.log("Error Data: " + data + "\nStatus: " + status );
                alert("Unable to locate nearest store from your location" );
            }
        });
     };

    return {
        fetchStoreFromPosition: function() {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(fetchStore);
            } else  {
                $("#storeNearby").html("Geolocation is not supported by this browser.");
            }
        },

        frameRadioImage: function(container, elt) {
           var imgElts = $("#"+container).find("img").get();
           var i;
           for (i = 0; i < imgElts.length; i++) {
               if ($(imgElts[i]).attr('name') != elt) {
                   $(imgElts[i]).removeAttr('class');
               } else {
                   $(imgElts[i]).attr('class', imgSelectStyle);
               }
           }
        }
    }
}());


