
(function(){
    var handler = function(request) {
        if (!(request === null)) {
            process.stdout.write(request);
        }
    }
    var worker = Elm.worker(Elm.Main
                            , {responses: []}
                           );
    worker.ports.requests.subscribe(handler);

    // Start msg
    worker.ports.responses.send([]);
})();
}
jsdom.env('<p>bleh</p>', [], callback);
