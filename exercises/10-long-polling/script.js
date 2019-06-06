async function loop() {
    var status = "Loading...",
        node = document.getElementById("status");

    try {
        while (1) {
            node.innerText = status;
            var res = await fetch("/get-status?status=" + status);
            if (res.ok) {
                status = await res.text();
            } else {
                throw "Invalid status code";
            }
        }
    }
    catch (e) {
        node.innerText = e;
    }
}
loop();
