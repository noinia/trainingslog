function flotSelection(selectF, unselectF) {
    $(document).ready(function() {
        $("#graphArea").bind( "plotselected", function( event, ranges ) {
            selectF(ranges.xaxis.from,ranges.xaxis.to)
            // similar for yaxis - with multiple axes, the extra ones are in
            // x2axis, x3axis, ...
        });

        $("#graphArea").bind( "plotunselected", function( event ) {
            unselectF()
        });
    });
}
