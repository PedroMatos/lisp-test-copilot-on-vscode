# Makes the one time setup.
# ci/provide-lisp.sh : Installs SBCL (Lisp) if not available and configures quicklisp.
# ci/provide-lisp-LSP.sh : Init LSP using SBCL.

cd ci
./provide-lisp.sh
./provide-quicklisp.sh
./provide-lisp-LSP.sh
cd ..
