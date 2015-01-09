Elm.Native.IO = {};
Elm.Native.IO.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.IO = localRuntime.Native.IO || {};
    if (localRuntime.Native.IO.values) {
        return localRuntime.Native.IO.values;
    }

    var putS = function(s) {
        process.stdout.write(s);
    };

    return localRuntime.Native.IO.values = {
        putS: putS,
        exit: process.exit
    };
};
