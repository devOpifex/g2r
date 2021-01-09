const crosstalkFilter = (c, ctFilter) => {
  ctFilter.on("change", function(e) {

    // reset
    if (e.sender !== ctFilter) {
      c.views.forEach((v) => {
        v.filter('CROSSTALK_KEYS', null)
      })
    }

    // filter
    let filtered = e.value;
    c.views.forEach((v) => {

      if(filtered == null || filtered.length == 0){
        v.filter('CROSSTALK_KEYS', null)
      } else {
        v.filter('CROSSTALK_KEYS', (key) => filtered.includes(key))
      }

    })

    // apply filter
    c.render();

  });
}

const crosstalkSelect = (c, ctSelection, opts) => {

  let firstRun = true,
      originalAttrs = [];
  
  ctSelection.on("change", (e) => {

    let indices = [], 
        selected = e.value;

    // if first run collect original colors
    if(firstRun){
      c.views.map((v) => {
        
        opts.map((opt) => {
          
          let values = [];
          v.geometries[0].elements.map((el) => {
            values.push(el.shape.attrs[opt.attribute])
          })

          originalAttrs.push({attribute: opt.attribute, values: values})

        })

      })
    }
    
    // get selected indices
    selected.forEach((sel) => {
      let found = c.getData().findIndex((data) => data["CROSSTALK_KEYS"] == sel)
      if(found >= 0)
        indices.push(found)
    })

    console.log(indices);
    console.log(opts);

    if(indices.length === 0){
      c.views.map((v, vindex) => {
        v.geometries[0].elements.map(function(el, index){
  
          opts.map((opt) => {
            el.shape.attrs[opt.attribute] = originalAttrs[vindex].values[index];
          })
    
        })
      })

      c.render();

      return ;
    }

    c.views.map((v, vindex) => {
      v.geometries[0].elements.map(function(el, index){

        if(indices.includes(index)){

          opts.map((opt) => {
            if(opt.on !== 'none')
              el.shape.attrs[opt.attribute] = opt.on
          })

        } else {

          opts.map((opt) => {
            if(opt.off === 'none')
              el.shape.attrs[opt.attribute] = originalAttrs[vindex].values[index];
            else
              el.shape.attrs[opt.attribute] = opt.off
          })

        }
  
      })
    })

    firstRun = false;

    // apply changes
    c.render();
  })
}

export { crosstalkFilter, crosstalkSelect }
