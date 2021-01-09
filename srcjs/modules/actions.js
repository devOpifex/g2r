const actions = (c, actions) => {

  if(actions === undefined)
    return ;

  if(actions.length === 0)
    return ;

  actions.forEach((action) => {

    if(action.name == "toggle-visibility"){
      let btn = document.getElementById(action.id);
      
      if(btn === null)
        return ;

      btn.addEventListener("click", () => {
        let current = c.visible;
        c.changeVisible(!current);
      })

    }

    if(action.name == "select-data"){
      let select = document.getElementById(action.id);

      if(select === null)
        return ;

      // initial select
      c.changeData(action.datasets[select.value]);

      select.addEventListener("change", () => {
        c.changeData(action.datasets[select.value]);
        c.render();
      })  

    }

    if(action.name == "filter-data"){
      let func;
      let slider = document.getElementById(action.id);

      if(slider === null)
        return ;

      if(["double", "integer"].includes(action.type))
        func = new Function(
          "return (val) => val " + action.op + " parseFloat(document.getElementById('" + action.id + "').value);"
        )();
      else 
        func = new Function(
          "return (val) => val " + action.op + " document.getElementById('" + action.id + "').value;"
        )();

      // initial value
      c.views.forEach((v) => {
        v.filter(action.field, func);
      });

      // listen to changes in slider
      slider.addEventListener("change", () => {
        c.views.forEach((v) => {
          v.filter(action.field, func);
        })
        c.render();
      })  

    }

  })

}

export { actions }