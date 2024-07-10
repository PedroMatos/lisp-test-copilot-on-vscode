# Check if SBCL is installed before attempting to install Quicklisp
if [ -z "$(command -v sbcl)" ]; then
    echo "SBCL (Lisp) is not installed. Please install SBCL before installing Quicklisp."
else
    echo "Installing Quicklisp..."
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
fi