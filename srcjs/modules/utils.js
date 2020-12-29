const check = (options, name) => {
  return typeof options[name] == 'object' && options[name].length == 2;
}

export { check }