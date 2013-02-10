function flotSelection(f) {
    $(document).ready(function() {
        $("#graphArea").bind( "plotselected", function( event, ranges ) {
            f(ranges.xaxis.from,ranges.xaxis.to)
            // similar for yaxis - with multiple axes, the extra ones are in
            // x2axis, x3axis, ...
        });
    });
}
