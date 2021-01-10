function getInputValue(id){
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
