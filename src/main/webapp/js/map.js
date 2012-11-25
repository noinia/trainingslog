

function drawMap(area,locations) {
    $(document).ready(function () {


        var mapOptions = {
          center: locations[0],
          zoom: 12,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };

        var map = new google.maps.Map(area,
            mapOptions);

        var flightPath = new google.maps.Polyline({
            path: locations,
            strokeColor: "#FF0000",
            strokeOpacity: 1.0,
            strokeWeight: 2
        });

        flightPath.setMap(map);

        // alert('woei2');
    });
}



// function drawMap(area, locations) {
//     $(document).ready(function() {
//         $(area).gmap().bind('init', function(ev, map) {
//             // $(area).gmap('addMarker',
//             //              {'position': '57.7973333,12.0502107',
//             //               'bounds': true});

//             var points  = [];
//             for(i=0; i<locations.loc.length; i++) {
//                 var point = new google.maps.LatLng(locations.loc[i].lat,locations.loc[i].lng);
//                 points.push(point);
//             }

//             var flightPlanCoordinates = [
//                 new google.maps.LatLng(37.772323, -122.214897),
//                 new google.maps.LatLng(21.291982, -157.821856),
//                 new google.maps.LatLng(-18.142599, 178.431),
//                 new google.maps.LatLng(-27.46758, 153.027892)
//             ];


//             // $(area).gmap('addMarker',
//             //              {'position': '57.7973333,12.0502107',
//             //               'bounds': true});

//             // $(area).gmap('addMarker',
//             //              {'position': points[0],
//             //               'bounds': true});


//             $('#map_canvas').gmap({'callback':function() {
//                 $('#map_canvas').gmap('addShape', 'Circle', { 'strokeColor': "#FF0000", 'strokeOpacity': 0.8, 'strokeWeight': 2, 'fillColor': "#FF0000", 'fillOpacity': 0.35, 'center': new google.maps.LatLng(58.12, 12.01), 'radius': 2000 });
//             }});

//             // $(area).gmap('addShape', 'Polyline',
//             //              {  'path': flightPlanCoordinates,
//             //                 'strokeColor': "#FF0000",
//             //                 'strokeOpacity': 1.0,
//             //                 'strokeWeight': 3
//             //              });


//         });
//     });
// }




//         // var myOptions = {
//         //     zoom: 12,
//         //     mapTypeId: google.maps.MapTypeId.ROADMAP
//         // };

//         // var mapArea = $(area);
//         // var map = new google.maps.Map(mapArea.get(), myOptions);
//         // initialLocation = new google.maps.LatLng(locations.loc[0].lat, locations.loc[0].lng);
//         // map.setCenter(initialLocation);
//         // for(i=0; i<locations.loc.length; i++) {
//         //     var point = new google.maps.LatLng(locations.loc[i].lat,locations.loc[i].lng);

//         //     var marker = new google.maps.Marker({position: point,
//         //                                          title: locations.loc[i].title });
//         //     marker.setMap(map);
//         // }
//         // alert("woei");
