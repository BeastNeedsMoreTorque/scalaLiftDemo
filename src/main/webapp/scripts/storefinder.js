"use strict";

// Global variable, singleton storeFinder
// API:
// <localhost:port>/stores/lat/<lat>/lng/<lng> gives closest store from coordinate(lat,lng).
// Currently supported Store  (see lcboapi.com): id, name, is_dead, latitude, longitude, distance_in_meters (from specified location), address_line_1, city
var storeFinder = (function() {
  var defaultOntarioLocation = new google.maps.LatLng(43.647219, -79.3789987); // Bay & Front LCBO address.
  var kmsPerLat = 111;
  var kmsPerLng = 78.4;
  var mapCanvas = document.getElementById('map-canvas');
  var map;
  var markers = [];
  var stores = [];
  var userMarker;
  var userLocation;
  var storeDistance;  // could also get storeDuration if we wanted.
  var distMatrixService = new google.maps.DistanceMatrixService();
  var directionsService = new google.maps.DirectionsService();
  var directionsDisplay;
  var theSelectedStore = null;

  var distanceByGeo = function(latLng1, latLng2) {
    var x = kmsPerLat * (latLng1.lat()-latLng2.lat());
    var y = kmsPerLng * (latLng1.lng()-latLng2.lng());

    return (Math.sqrt(Math.pow(x,2) + Math.pow(y,2)));
  };

  var closestStore = function(latLng) {
    var bestDistance = +Infinity;
    var dist = bestDistance;
    var closest = null;
    stores.forEach(
      function (store, index, array) {
        var storeLatLng = new google.maps.LatLng(store.latitude, store.longitude)
        dist = distanceByGeo(latLng, storeLatLng);
        if (dist < bestDistance) {
          closest = store;
          bestDistance = dist;
        }
      }
    );
    return closest;
  }

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
        clearMarkers();
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
    theSelectedStore = closestStore(userLatLng);
    if (theSelectedStore != null) {
      var closestLatLng = new google.maps.LatLng(theSelectedStore.latitude, theSelectedStore.longitude)
      if (storeSelectedByApp == true) {
        var closestMarker = new google.maps.Marker({position:closestLatLng,map:map,title:"Closest store",icon:"http://maps.google.com/mapfiles/ms/icons/blue-dot.png"});
      }

      evaluateDistance(closestLatLng);
      getDirections(closestLatLng);

      $("#storeNearby").html('Selected LCBO store:');
      $("#storeName").html(theSelectedStore.name);
      $("#storeAddressLine1").html(theSelectedStore.address_line_1);
      $("#storeCity").html(theSelectedStore.city);
      $("#storeLat").html(theSelectedStore.latitude);
      $("#storeLon").html(theSelectedStore.longitude);
      $("#storeAttributesTbl").show();
    } else {
      $("#storeNearby").html("Downtown Toronto LCBO (user location unavailable)");
      $("#storeAttributesTbl").hide();
    }
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
  var addMarker = function(name, location) {
    var marker = new google.maps.Marker({
      title: name,
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

    getTheSelectedStore: function() {
      if (theSelectedStore == null) return -1;
      return theSelectedStore.idField;
    },

    fetchAllStores: function() {
      $.ajax({
        url: '/stores',
        type: 'GET',
        success: function(data, status){
          function createMarker(element, index, array) {
            var latlng = new google.maps.LatLng(element.latitude, element.longitude);
            addMarker(element.name, latlng);
          }
          data.forEach(createMarker);
          stores = data;
          storeFinder.fetchStoreFromPosition();
        },
        error: function(data, status){
          console.log("Error Data: " + data.responseText + "\nStatus: " + status );
          alert(data.responseText );
        }
      });
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
  storeFinder.fetchAllStores();
});


