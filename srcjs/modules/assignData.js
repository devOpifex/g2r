const assignData = (view, v, x) => {
  if(v.data)
    view.data(v.data);
  else
    view.data(x.data);
}

export { assignData };
