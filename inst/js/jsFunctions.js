//creating external functions?

filter = function(label, type, range) {

  if (type == "points") {
    d3.select(label).selectAll('use')
    .data(tab)
    .filter(function(d) {return range.min <= d.x && d.x <= range.max })
    .style("fill", "red")
  }

}


highlight = function(label, fillColor, strokeColor, filterOn) {
  var label = document.getElementById(label);
  d3.select(label).selectAll('polygon')
  .on('mouseover', function(d) {
    var el = d3.select(this);
    if (fillColor != null) {
      el.setAttribute('fill', fillColor);
      el.setAttribute('fill-opacity', 1);
    } else {
      el.setAttribute('stroke', strokeColor);
    }
  })

  // for filtering points - relative to other plot:
  if (filterOn == true) {
    filter(panel2, "points", range)
  }

}


highlightPoints = function(label, fillColor) {
  var label = document.getElementById(label);
  d3.select(label).selectAll('use')
    .on('mouseover', function(d) {
            var el = d3.select(this);
            el.setAttribute('fill', fillColor);
            el.setAttribute('fill-opacity', 1);
    })
    .on('mouseout', function(d) {
      var el = d3.select(this);
          el.setAttribute('fill-opacity', 0);
    })
}
