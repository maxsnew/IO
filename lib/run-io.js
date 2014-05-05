var run_io = function(Elm) {
    var stdin = process.stdin;
    var handle = function(request) {
        // Debugging:
        // console.log("Bleh: %j", request);
        switch(request.ctor) {
        case 'Put':
            process.stdout.write(request.val);
            break;
        case 'Get':
            stdin.resume();
            break;
        case 'Exit':
            process.exit(request.val);
        }
    }
    var handler = function(reqs) {
        for (var i = 0; i < reqs.length; i++) {
            handle(reqs[i]);
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
};

exports.run_io = run_io;
