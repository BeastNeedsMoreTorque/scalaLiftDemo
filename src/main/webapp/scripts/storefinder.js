"use strict";

// Global variable, singleton storeFinder
// API:
// <localhost:port>/stores/lat/<lat>/lng/<lng> gives closest store from coordinate(lat,lng).
// Currently supported Store  (see lcboapi.com): id, name, is_dead, latitude, longitude, distance_in_meters (from specified location), address_line_1, city
var storeFinder = (function() {
  var defaultOntarioLocation = new google.maps.LatLng(43.647219, -79.3789987); // Bay & Front LCBO address.
  var mapCanvas = document.getElementById('map-canvas');
  var map;
  var markers = [];
  var userMarker;
  var userLocation;
  var storeDistance;  // could also get storeDuration if we wanted.
  var closestStoreName = 'Unknown Store';
  var distMatrixService = new google.maps.DistanceMatrixService();
  var directionsService = new google.maps.DirectionsService();
  var directionsDisplay;

  var fetchStore = function(userLatLng, userLocationAvailable, storeSelectedByApp) {
    var myOptions = {
      center:userLatLng,
      zoom:12,
      mapTypeId:google.maps.MapTypeId.HYBRID,
      mapTypeControl:true
    }
    if (storeSelectedByApp == true) {
      map = new google.maps.Map(mapCanvas, myOptions);
      map.addListener('bounds_changed', function() {
        clearMarkers()
        if (this.zoom > 10) {
          showMarkers();
        }
        else {
          clearMarkers();
        }
      });
      directionsDisplay = new google.maps.DirectionsRenderer();
      directionsDisplay.setMap(map);
      userMarker = new google.maps.Marker({position:userLatLng,map:map,title:"Current Location",icon:"http://maps.google.com/mapfiles/ms/icons/green-dot.png"});
    }


    // Show particulars of nearby store in storeNearby and also show that point in a google map.
    $.ajax({
      // liftweb interprets periods in special way for suffixes in JSON calls (req parsing), so we escape them by using commas instead.
      url: '/stores/lat/' + userLatLng.lat().toString().replace('.',  ',') + '/lng/' + userLatLng.lng().toString().replace('.', ','),
      type: 'GET',
      success: function(data, status){
        var latlng = new google.maps.LatLng(data.latitude, data.longitude)
        var title = 'Downtown Toronto Liquor Store';
        if (userLocationAvailable) {
          $("#storeNearby").html('Selected LCBO store:');
          title = "Selected Store";
          $("#storeName").html(data.name);
          closestStoreName = data.name;
          $("#storeAddressLine1").html(data.address_line_1);
          $("#storeCity").html(data.city);
          $("#storeLat").html(data.latitude);
          $("#storeLon").html(data.longitude);
          $("#storeAttributesTbl").show();
        } else {
          $("#storeNearby").html("Downtown Toronto LCBO (user location unavailable)");
          $("#storeAttributesTbl").hide();
        }
        if (storeSelectedByApp == true) {
          var closestMarker = new google.maps.Marker({position:latlng,map:map,title:title,icon:"http://maps.google.com/mapfiles/ms/icons/blue-dot.png"});
          closestMarker.addListener('click', storeFinder.storeClickCB);
          fetchAllStores();
          getDirections(latlng);
        }
      },
      error: function(data, status){
        console.log("Error Data: " + data.responseText + "\nStatus: " + status );
        alert(data.responseText );
      }
    });
  };

  var fetchAllStores = function() {
    $.ajax({
      url: '/stores',
      type: 'GET',
      success: function(data, status){
        function createMarker(element, index, array) {
          if (element.name != closestStoreName) {
            var latlng = new google.maps.LatLng(element.latitude, element.longitude);
            addMarker(latlng);
          }
        }
        data.forEach(createMarker);
      },
      error: function(data, status){
        console.log("Error Data: " + data.responseText + "\nStatus: " + status );
        alert(data.responseText );
      }
    });
  };

  var evaluateDistance = function(latLng) {
    distMatrixService.getDistanceMatrix(
    {
      origins: [userLocation],
      destinations: [latLng],
      travelMode: google.maps.TravelMode.DRIVING
    }, storeFinder.distMatrixCB);
  };

  var getDirections = function(latLng) {
   var request = {
      origin: userLocation,
      destination: latLng,
      travelMode: google.maps.TravelMode.DRIVING
    };
    directionsService.route(request, function(result, status) {
      if (status == google.maps.DirectionsStatus.OK) {
        directionsDisplay.setMap(null);
        directionsDisplay.setMap(map);
        directionsDisplay.setDirections(result);
      }
    });
  };

  var fetchStoreNearUser = function(position) {
    userLocation = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
    fetchStore( userLocation, true, true);
  };

  var showGeoError = function(error) {
    userLocation = defaultOntarioLocation;
    fetchStore(defaultOntarioLocation, false, true);
  };

  // Adds a marker to the map and push to the array.
  var addMarker = function(location) {
    var marker = new google.maps.Marker({
      position: location,
      map: map
    });
    marker.addListener('click', storeFinder.storeClickCB);
    markers.push(marker);
  };

  // Sets the map on all markers in the array.
  var setMapOnAll = function (map) {
    for (var i = 0; i < markers.length; i++) {
      markers[i].setMap(map);
    }
  };

  // Removes the markers from the map, but keeps them in the array.
  var clearMarkers= function() {
    setMapOnAll(null);
  };

  // Shows any markers currently in the array.
  var showMarkers = function() {
    setMapOnAll(map);
  };

  // Deletes all markers in the array by removing references to them.
  var deleteMarkers = function() {
    clearMarkers();
    markers = [];
  };

  return {
    storeClickCB: function(e) {
      fetchStore(e.latLng, true, false);
      evaluateDistance(e.latLng);
      getDirections(e.latLng);
    },

    distMatrixCB: function(response, status) {
      if (status == google.maps.DistanceMatrixStatus.OK) {
        var origins = response.originAddresses;
        var destinations = response.destinationAddresses;
        if(origins.length > 0) {
          var results = response.rows[0].elements;
          if(results.length > 0 ) {
            var element = results[0];
            storeDistance = element.distance.text;
            $("#storeDistance").html(storeDistance);
          }
        }
      }
    },

    fetchStoreFromPosition: function() {
      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(fetchStoreNearUser, showGeoError);
      } else  {
        $("#storeNearby").html("Geolocation is not supported by this browser.");
        fetchStore(defaultOntarioLocation, false, true);
      }
    }
  }
}());

$(document).ready(function(){
  storeFinder.fetchStoreFromPosition();
});


