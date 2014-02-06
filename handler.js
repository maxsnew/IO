
(function(){
    var handler = function(request) {
        // Debugging:
        // console.log("Bleh: %j", request);
        if (request.mPut.ctor === "Just") {
            process.stdout.write(request.mPut._0);
        } else if (request.mExit.ctor === "Just") {
            process.exit(request.mExit._0);
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
