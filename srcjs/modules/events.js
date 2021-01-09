const captureEvents = (c, el, options) => {

  if(options === undefined)
    return ;

  let events = addCallback(options, el.id)

  events.forEach(function(event){
    if(event.when == 'on')
      c.on(event.event, event.callback)

    if(event.when == 'once')
      c.once(event.event, event.callback)

    if(event.when == 'off')
      c.off(event.event, event.callback)
  })
  
}

// Add default callback if missing
const addCallback = (events, id) => {
  let newEvents = events.map((ev) => {
    if(ev.callback == undefined){
      ev.callback = makeShinyInput(id, ev.event)
    }
    return ev;
  })

  return newEvents;
}

// Shiny input factory
const makeShinyInput = (id, event) => {
  let inputName = event.replaceAll(":", "_");
  function cb(ev){
    Shiny.setInputValue(id + '_' + inputName, ev.data, {priority: 'event'});
  }

  return cb;
}

export { captureEvents };
