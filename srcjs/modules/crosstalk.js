const crosstalkFilter = (c, ctFilter) => {
  ctFilter.on("change", function(e) {

    console.log(e);

    // reset
    if (e.sender !== ctFilter) {
      c.views.forEach((v) => {
        v.filter('CROSSTALK_KEYS', null)
      })
    }

    // filter
    let filtered = e.value;
    c.views.forEach((v) => {

      if(filtered.length == 0){
        v.filter('CROSSTALK_KEYS', null)
      } else {
        v.filter('CROSSTALK_KEYS', (key) => filtered.includes(key))
      }

    })

    // apply filter
    c.render();

  });
}

const crosstalkSelect = (c, ctSelection, stroke = "black", fill = "none") => {

  let firstRun = true, // to set/reset colors;
      strokeColors = [],
      fillColors = []; 
  
  ctSelection.on("change", (e) => {

    let indices = [], 
        selected = e.value;

    // if first run collect original colors
    if(firstRun){
      c.views[0].geometries[0].elements.map(function(el){
        strokeColors.push(el.shape.attrs.stroke);
        fillColors.push(el.shape.attrs.fill);
      })
    }
    
    // get selected indices
    selected.forEach((sel) => {
      let found = c.getData().findIndex((data) => data["CROSSTALK_KEYS"] == sel)
      if(found >= 0)
        indices.push(found)
    })

    // loop over elements of plot to color
    c.views[0].geometries[0].elements.map(function(el, index){

      if(indices.includes(index)){
        if(stroke != "none")
          el.shape.attrs.stroke = stroke;
        if(fill != "none")
          el.shape.attrs.fill = fill;
      } else {
        el.shape.attrs.stroke = strokeColors[index];
        el.shape.attrs.fill = fillColors[index];
      }

    })

    firstRun = false;

    // apply changes
    c.render();
  })
}

export { crosstalkFilter, crosstalkSelect }
