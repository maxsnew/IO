Elm.Native.NativeCom = {};
Elm.Native.NativeCom.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.NativeCom = localRuntime.Native.NativeCom || {};
    if (localRuntime.Native.NativeCom.values) {
    return localRuntime.Native.NativeCom.values;
    }

    /* Elm imports */
    var List = Elm.Native.List.make(localRuntime);
    var Maybe = Elm.Maybe.make(localRuntime);
    var NS = Elm.Native.Signal.make(localRuntime);
    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    /* Node.js imports */
    var fs = require('fs');
    var stdin = process.stdin;

    var sendResponseString = function(str) {
        var value = Maybe.Nothing;
        if (str !== null && str.length > 0) {
            value = Maybe.Just(str);
        }
        setTimeout(function() {
            localRuntime.notify(responsesSignal.id, value);
        }, 0);
    }

    var responsesSignal = NS.input('IO.NativeCom.responses', Maybe.Nothing);
    stdin.on('data', function(chunk) {
        stdin.pause();
        sendResponseString(chunk.toString());
    })

    var sendRequestBatch = function(list) {
        var requests = List.toArray(list);
        if (requests.length == 0) {
            return Task.succeed(Utils.Tuple0);
        }

        return Task.asyncFunction(function(callback) {

            requests.forEach(doRequest);

            var lastReq = requests[requests.length - 1];
            if (lastReq.ctor !== 'Get') {
                // if we are not waiting for stdin,
                // trigger the next IO requests immediately
                sendResponseString(null);
            }

            return callback(Task.succeed(Utils.Tuple0));
        });
    }

    var doRequest = function(request) {
        switch(request.ctor) {
            case 'Put':
                process.stdout.write(request._0);
                break;
            case 'Get':
                stdin.resume();
                break;
            case 'Exit':
                process.exit(request._0);
                break;
            case 'WriteFile':
                fs.writeFileSync(request._0.file, request._0.content);
                break;
            case 'Init':
                // trigger the initial IO requests
                sendResponseString(null);
                break;
        }
    }

    return localRuntime.Native.NativeCom.values = {
        sendRequestBatch: sendRequestBatch,
        responses: responsesSignal
    };
};
