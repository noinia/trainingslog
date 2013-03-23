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
    // $(document).ready(function () {
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
                    }
    // })
}


/**
 *  graphsData = { graphArea : String
 *               , plot      : Flot Plot object
 *               }
 *  mapData    = { currentPosition : LatLng
 *               , map             : google.maps.Map object
 *               , locations       : Array[Location objects]
 *               }
 * locationObject = {position : LatLng , time: Double }
 */
function linkMapAndGraph(graphsData, mapData) {
    $(document).ready(function () {

        function findEdgeContaining(vertices, p) {
            // lookup which edge contains this point
            for (var i=0 ; i < vertices.length - 1 ; i++) {
                var e = new google.maps.Polyline({path : [vertices[i],vertices[i+1]]})
                if (google.maps.geometry.poly.isLocationOnEdge(e.latLng,e))
                    return e
                delete e
            }
            return null
        }

        alert("graphs: "+graphsData.plot)
        alert("map: " + mapData.map)

        // Graphs -> Map
        $(graphsData.graphArea).bind("plothover", function(event, position, item) {
            var axes = graphsData.plot.getAxes()
            var t = Math.min(Math.max(axes.xaxis.min, position.x), axes.xaxis.max)
            placeMarker(mapData.currentPosition,Math.round(t),mapData.locations)
        })

        // Map -> Graphs
        google.maps.event.addListener(path,"mouseover", function(e) {
            currentPosition.setPosition(e.latLng)
            var e = findEdgeContaining(positions, e.latLng)
            alert(e)
        })
    })
}
