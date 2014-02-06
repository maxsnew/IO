
(function(){
    var stdin = process.stdin;
    var handler = function(request) {
        // Debugging:
        //console.log("Bleh: %j", request);
        if (!(request.mPut === null)) {
            process.stdout.write(request.mPut);
        } else if (!(request.mExit === null)) {
            process.exit(request.mExit);
        } else if (request.mGet) {
            stdin.resume();
        }
    }
    var worker = Elm.worker(Elm.Main
                            , {responses: null }
                           );
    worker.ports.requests.subscribe(handler);
    
    // Read
    stdin.on('data', function(chunk) {
        //console.log('Got' + chunk);
        stdin.pause();
        worker.ports.responses.send(chunk.toString());
    })

    // Start msg
    worker.ports.responses.send(null);
})();
}
jsdom.env('<p>bleh</p>', [], callback);
