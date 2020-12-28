const captureEvents = (c, options) => {

  if(!options.events)
    return ;

  options.events.forEach(function(event){
    if(event.when == 'on')
      c.on(event.event, event.callback)

    if(event.when == 'once')
      c.once(event.event, event.callback)

    if(event.when == 'off')
      c.off(event.event, event.callback)
  })
}

export { captureEvents };
