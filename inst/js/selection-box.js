// A more generalized version:
  var zoomBox = {};
  var selectRect = document.getElementById("selectRect.1.1");

// co-ordinate conversion for svg relative to where it is on the page:
convertCoord = function(svg, evt) {
  var pt = svg.createSVGPoint();
  pt.x = evt.pageX;
  pt.y = evt.pageY;
  return pt.matrixTransform(svg.getScreenCTM().inverse());
}

MouseDown = function(evt) {
  var pt  = convertCoord(svg, evt);
    evt.stopPropagation();
    zoomBox["startX"] = pt.x;
    zoomBox["startY"] = pt.y;
    zoomBox["isDrawing"] = true;
   selectRect.setAttribute('points',  zoomBox["startX"] + ',' + zoomBox["startY"]);
};


MouseUp = function(evt) {
  svg.style.cursor = "default";
  var pt  = convertCoord(svg, evt);
  evt.stopPropagation();
  zoomBox["endX"] = pt.x;
  zoomBox["endY"] = pt.y;
  zoomBox["isDrawing"] = false;
};

MouseDrag = function(evt) {
    if(zoomBox["isDrawing"]) {
        svg.style.cursor = "crosshair";
        evt.stopPropagation();
        var pt  = convertCoord(svg, evt);
        zoomBox["endX"] = pt.x;
        zoomBox["endY"] = pt.y;

        //Because the y-axis is inverted in the plot - need to invert the scale
        var tVal = document.getElementsByTagName('g')[0].getAttribute('transform').substring(13, 16);

         // for rectangles with positive height, positive width
        if(zoomBox["startX"] < zoomBox["endX"]) {
        var x1 = zoomBox["startX"];
        var x2 = zoomBox["endX"];
      } else {
        var x1 = zoomBox["endX"];
        var x2 = zoomBox["startX"];
      }

      // for rectangles with opposite directions ('negative' widths, heights)
      if (zoomBox["startY"] < zoomBox["endY"]) {
        var y1 = tVal - zoomBox["startY"] - (zoomBox["endY"]-zoomBox["startY"]);
        var y2 = y1 + (zoomBox["endY"]-zoomBox["startY"]);
      } else {
        var y1 = tVal - zoomBox["endY"] - (zoomBox["startY"]-zoomBox["endY"]);
        var y2 = y1 + (zoomBox["startY"]-zoomBox["endY"]);
      }

        selectRect.setAttribute('points', x1 + ',' + y1 + " " + x1 + ',' + y2 + ' '
                                  + x2 + ',' + y2 + ' ' + x2 + ',' + y1);

        var selected = [];
        var num = document.getElementById(pointId).childElementCount;
        //console.log(num);
        for (i =1; i <= num; i++) {
          var point = document.getElementById(pointId + '.' + i);

          //obtain x, y values
          var x = point.x.baseVal.value;
          var y = point.y.baseVal.value;

          //points that lie within the  boundary box drawn:
          if((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
            point.setAttribute('fill-opacity', '1');
            selected.push(i);
           } else {
             point.setAttribute('fill-opacity', '0.5');
           }
         }

         //SHINY VERSION: return selected rows to Shiny?
         //Shiny.onInputChange("selectedPoints", selected);

         //for DOM version 0.4 and lower:
         svg.setAttribute('data-select', selected);
         RDOM.Rcall("hello", this, [ "ptr" ], null);

        //console.log(this); - test what 'this' returns

        //test DOM version 0.5:
        //RDOM.Rcall("hello", selected, [ "JSON" ], null);

        }
};
