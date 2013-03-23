var myMapData

function placeMarker(m, t, locations) {
    var i = 0
    while( i < locations.length && locations[i].time <= t){
        i++;
    }
    m.setPosition(locations[i-1].position)
}

function zoomToFit(map, locs) {
    var bounds = new google.maps.LatLngBounds()
    for (var i=0 ; i < locs.length ; i++ )
        bounds.extend(locs[i])
    map.fitBounds(bounds)
}


function drawMap(area,locations) {
    var positions = locations.map(function (x) {return x.position})

    var map = new google.maps.Map(area, {
        mapTypeId: google.maps.MapTypeId.ROADMAP
    })

    var path = new google.maps.Polyline({
        path         : positions
      , strokeColor  : "#FF0000"
      , strokeOpacity: 1.0
      , strokeWeight : 2
      , map          : map
      // , editable     : true // we need this for the mouseover stuff.
    })

    zoomToFit(map,positions)

    var startMarker = new google.maps.Marker({
        map       : map
      , position  : positions[0]
      , title     : "Start"
    })
    var endMarker = new google.maps.Marker({
        map       : map
      , position  : positions[positions.length-1]
      , title     : "End"
    })

    var currentPosition = new google.maps.Marker({
        map       : map
      , position  : positions[0]
    })

    myMapData = { "map"             : map
                , "currentPosition" : currentPosition
                , "locations"       : locations
                , "path"            : path
                }
}


/**
 *  graphsData = { graphArea : String
 *               , plot      : Flot Plot object
 *               }
 *  mapData    = { currentPosition : LatLng
 *               , map             : google.maps.Map object
 *               , locations       : Array[Location objects]
 *               , path            : Polyline object, the path we are displaying
 *               }
 * locationObject = {position : LatLng , time: Double }
 */
function linkMapAndGraph(graphsData, mapData) {
    $(document).ready(function () {

        function findEdgeContaining(vertices, p) {
            // lookup which edge contains this point
            for (var i=0 ; i < vertices.length - 1 ; i++) {
                var e = new google.maps.Polyline({ path : [vertices[i],vertices[i+1]]})
                if (google.maps.geometry.poly.isLocationOnEdge(p,e))
                    return e
                delete e
            }

            return null
        }

        var positions = mapData.locations.map(function (x) {return x.position})

        // Graphs -> Map
        $(graphsData.graphArea).bind("plothover", function(event, position, item) {
            var axes = graphsData.plot.getAxes()
            var t = Math.min(Math.max(axes.xaxis.min, position.x), axes.xaxis.max)
            placeMarker(mapData.currentPosition,Math.round(t),mapData.locations)
        })

        // Map -> Graphs
        // TODO: we need a faster/better solution here
        // google.maps.event.addListener(mapData.path,"mouseover", function(evt) {
        //     mapData.currentPosition.setPosition(evt.latLng)
        //     // var e = findEdgeContaining(positions, evt.latLng)
        //     alert(evt.edge)
        // })
    })
}
