(ns cryptovault
  (:require (clojure.java [io :as io]))
  (:use [io :only [IOFactory make-reader make-writer]])
  (:import (java.security KeyStore KeyStore$SecretKeyEntry)
           (javax.crypto KeyGenerator Cipher CipherOutputStream
                         CiphterInputStream)
           (java.io FileOutputStream)))

(defprotocol Vault
  (init-vault [vault])
  (vault-output-stream [vault])
  (vault-input-stream [vault]))

(deftype CryptoVault [filename keystore password]
  Vault
  (init-vault [vault])
  (vault-input-stream [vault])
  (vault-output-stream [vault])
  
  IOFactory
  (make-reader [vault]
    (make-reader (vault-input-stream vault)))
  (make-writer [vault]
    (make-writer (vault-output-stream vault))))

  
  
