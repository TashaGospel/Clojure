(ns sax
  (:import (org.xml.sax InputSource)
           (org.xml.sax.helpers DefaultHandler)
           (java.io StringReader)
           (javax.xml.parsers SAXParserFactory)))

(def print-element-handler
  (proxy [DefaultHandler] []
    (startElement [uri local qname atts]
      (println (format "Saw element: %s" qname)))))

(defn demo-sax-parse [source handler]
  (.. SAXParserFactory newInstance newSAXParser
      (parse (InputSource. (StringReader. source)) handler)))
