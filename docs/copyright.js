// copyright.js
// (c) 2020 Esa Pulkkinen <esa.pulkkinen@iki.fi>
// This javascript reads schema.org attribute annotated HTML
// And produces additional presentation details on the data.
//
var structured = document.getElementById('structured');
var results = structured.querySelectorAll(':scope table tr[itemtype^="http://schema.org/"] td details');
for (i = 0; i < results.length; ++i) {
    var copyrightbox = results[i];
    var fields=copyrightbox.querySelectorAll(':scope summary[itemprop],div[itemprop], td[itemprop], tr[itemprop], tbody[itemprop]');
    for (j = 0; j < fields.length; ++j) {
        if (fields[j]) {
            var name = fields[j].getAttribute('itemprop');
            var row = document.createElement("TR");
            row.style.display='flex';
            if (name != "name") {
                var header = document.createElement("TH");
                header.padding="5px";
                header.textAlign="right";             
                header.textContent = name + ":";
                row.appendChild(header);
            }
            var col = document.createElement("TD");
            if (name != "name") { col.style.colspan = 2; }
            col.padding = "5px";
            col.textAlign="left";
            if (name == "about" || name == "url") {             
                var link = document.createElement("A");
                link.setAttribute("href",fields[j].textContent);
                link.textContent = fields[j].textContent;      
                col.appendChild(link);
            } else {
                col.textContent = fields[j].textContent;
            }
            row.appendChild(col);
            fields[j].replaceChild(row, fields[j].childNodes[0])
        }
    }
}
