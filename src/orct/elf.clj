;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; elf.cljs - parser for elf data

(ns orct.elf
  (:use [orct.utils]
        [clojure.java.io])
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))


(defn parse-elf32-header
  "returns elf header as map with the following name space qualified keywords for binary content s:

     ::e_ident       - ELF Identification
     ::e_type        - object file type: 0=No file type, 1=reloc, 2=exec, 3=shared obj, 4=core
     ::e_machine     - architecture: 0=no, 1=AT&T, 2=SPARC, 3=Intel, 4=Motorola 68K, 5=Motorola 88K
     ::e_version     - object file version: 0=invalid, 1=current
     ::e_entry       - virtual address to which the system first transfers control or zero
     ::e_phoff       - program header table's file offset in bytes
     ::e_shoff       - section header table's file offset in bytes
     ::e_flags       - processor-specific flags associated with the file
     ::e_ehsize      - ELF header's size in bytes
     ::e_phentsize   - size in bytes of one entry in the file's program header table
     ::e_phnum       - number of entries in program header table
     ::e_shentsize   - section header's size in bytes
     ::e_shnum       - number of entries in the section header table
     ::e_shstrndx    - section header table index of the entry associated with the sec name str table"
  [s]
  (proc-parse-struct
   [{::e_ident (fn [s] [(drop 16 s) (take 16 s)])}
    {::e_type rest-uint16-pair}
    {::e_machine rest-uint16-pair}
    {::e_version rest-uint32-pair}
    {::e_entry rest-uint32-pair}
    {::e_phoff rest-uint32-pair}
    {::e_shoff rest-uint32-pair}
    {::e_flags rest-uint32-pair}
    {::e_ehsize rest-uint16-pair}
    {::e_phentsize rest-uint16-pair}
    {::e_phnum rest-uint16-pair}
    {::e_shentsize rest-uint16-pair}
    {::e_shnum rest-uint16-pair}
    {::e_shstrndx rest-uint16-pair}]
   s))


(defn- parse-elf32-section-header
  "parsing one section header element"
  [s]
  (proc-parse-struct
   [{::sh_name rest-uint32-pair}
    {::sh_type rest-uint32-pair}
    {::sh_flags rest-uint32-pair}
    {::sh_addr rest-uint32-pair}
    {::sh_offset rest-uint32-pair}
    {::sh_size rest-uint32-pair}
    {::sh_link rest-uint32-pair}
    {::sh_info rest-uint32-pair}
    {::sh_addralign rest-uint32-pair}
    {::sh_entsize rest-uint32-pair}]
   s))


(defn- parse-elf32-section-headers-stage1
  "parsing of section header table list for given elf header h and elf binary content s.
   refer to function parse-elf32-section-headers for more detailed information."
  [h s]
  (let [sec-header-offset (::e_shoff h)
        sec-cnt-vec (range 0 (::e_shnum h))
        sec-elem-size (::e_shentsize h)
        sec-offset-vec (map #(+ sec-header-offset (* sec-elem-size %)) sec-cnt-vec)]
    (map #(parse-elf32-section-header (drop % s))
         sec-offset-vec)))


(defn- split-seq-at-null
  "splits elements not equal to zero off sequence
     (split-seq-at-null '(65 66 67 0)) -> '(65 66 67)"
  [s]
  (first
   (split-with (partial not= 0) s)))


(defn- associate-section-strings
  "associate section name string to section sequence secs with
   parsed elf header h and raw elf data sequence elf"
  [h secs elf]
  (let [strsec-idx (::e_shstrndx h)
        strsec (first (drop strsec-idx secs))
        strsec-offset (::sh_offset strsec)]
    (map
     (fn [sec]
       (let [name-offset (+ strsec-offset (::sh_name sec))]
         (assoc sec ::name
                (String. (byte-array
                          (split-seq-at-null (drop name-offset elf)))))))
     secs)))


(defn parse-elf32-section-headers
  "parsing of section header table list for given elf header h and elf binary content s.

   returns section header as map with the following qualified keywords:

     ::sh_name       - specifies the name of the section as index into sec header str table sec
     ::sh_type       - categorizes the section's contents and sematics
     ::sh_flags      - miscellaneous attributes
     ::sh_addr       - memory address at which the section's first bytes should reside or 0
     ::sh_offset     - bytes offset from the beginning of the file to first byte in section
     ::sh_size       - section's size in bytes
     ::sh_link       - holds a section header table index link
     ::sh_info       - extra information
     ::sh_addralign  - alignment constraints
     ::sh_entsize    - size in bytes for each table entry e.g. for symbol table

     ::name          - character string providing the section name"
  [h s]
  (let [secs (parse-elf32-section-headers-stage1 h s)]
    (associate-section-strings h secs s)))



(defn- parse-elf32-prog-header
 "parsing one program header element"
 [s]
  (proc-parse-struct
   [{::p_type rest-uint32-pair}
    {::p_offset rest-uint32-pair}
    {::p_vaddr rest-uint32-pair}
    {::p_paddr rest-uint32-pair}
    {::p_filesz rest-uint32-pair}
    {::p_memsz rest-uint32-pair}
    {::p_flags rest-uint32-pair}
    {::p_align rest-uint32-pair}]
   s))


(defn parse-elf32-prog-headers
  "parsing of prog header table list for given elf header h and elf binary content s.

   returns program header as map with the following qualified keywords for binary content s:

     ::p_type        - kind of segment
     ::p_offset      - offset from beginning of the file
     ::p_vaddr       - virtual address at which the first byte of the segment resides in mem
     ::p_paddr       - physical address if relevant
     ::p_filesz      - number of bytes in the file image
     ::p_memsz       - number of bytes int hte memory image
     ::p_flags       - flags relevant of the segment
     ::p_align       - alignment in memory"
  [h s]
  (let [prog-header-offset (::e_phoff h)
        prog-cnt-vec (range 0 (::e_phnum h))
        prog-elem-size (::e_phentsize h)
        prog-offset-vec (map #(+ prog-header-offset (* prog-elem-size %)) prog-cnt-vec)]
    (map #(parse-elf32-prog-header (drop % s))
         prog-offset-vec)))


(comment

  (write-to-file "samples/prim_copy" (byte-array (seq (read-file "samples/prim"))))

  (def s (seq (read-file "samples/alsaucm")))
  (def s (seq (read-file "samples/mcfg_sw_dt.mbn")))


  (def h (parse-elf32-header s))

  (def secs (parse-elf32-section-headers h s))
  (map #(println (::name %)) secs)

  (def progs (parse-elf32-prog-headers h s))
  (map #(println (::p_type %)) progs)

  )
