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

      if(select === null || select === undefined)
        return ;

      // initial select
      c.changeData(action.datasets[getInputValue(action.id)]);

      select.addEventListener("change", () => {
        let value = getInputValue(action.id);
        c.changeData(action.datasets[value]);
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
          "return (val) => val " + action.op + " parseFloat(getInputValue('" + action.id + "'));"
        )();
      else if (["boolean", "logical"].includes(action.type))
        func = new Function(
          "return (val) => String(val) " + action.op + " getInputValue('" + action.id + "');"
        )();
      else 
        func = new Function(
          "return (val) => val " + action.op + " getInputValue('" + action.id + "');"
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

const getInputValue = (id) => {
  var group = document.getElementById(id);

  // if it has a type it's an input
  if(group.type !== undefined)
    return group.value;

  // if it does not have a type it's a div
  // containing checkboxes or radio inputs
  let inputs = group.getElementsByTagName("input");

  for(let input of inputs){
    if(input.checked)
      return input.value
  }

  return ;
}


export { actions }