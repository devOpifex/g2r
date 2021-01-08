const actions = (c, actions) => {

  if(actions.length === 0)
    return ;

  actions.forEach((action) => {
    document.getElementById(action.btn).addEventListener("click", () => {
      let current = c.visible;
      c.changeVisible(!current);
    })
  })

}

export { actions }