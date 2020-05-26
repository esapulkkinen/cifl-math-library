// copyright.js
// (c) 2020 Esa Pulkkinen <esa.pulkkinen@iki.fi>
// This javascript reads schema.org attribute annotated HTML
// And produces additional presentation details on the data.
//
var structured = document.getElementById('structured');
var results = structured.querySelectorAll(':scope table tr[itemtype="http://schema.org/CreativeWork"] td details');
for (i = 0; i < results.length; ++i) {
    var copyrightbox = results[i];
    var fields=copyrightbox.querySelectorAll(':scope summary[itemprop],div[itemprop]');
    for (j = 0; j < fields.length; ++j) {
        if (fields[j]) {
            var name = fields[j].getAttribute('itemprop');
            if (name != 'name') {
                var row = document.createElement("TR");
                var header = document.createElement("TH");
                header.width="15%";          
                header.padding="5px";
                header.style.textAlign="right";             
                header.textContent = name + ":";
                row.appendChild(header);
            }
            var col = document.createElement("TD");
            col.padding = "5px";
            col.width="85%";           
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
