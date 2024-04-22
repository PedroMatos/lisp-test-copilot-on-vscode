# Install SBCL (Lisp) if not available.
# ------------------------------------------------------------------------------

if [ -z "$(command -v sbcl)" ]; then
    echo "Updating package database..."
    sudo apt-get update
    echo "Installing SBCL (Lisp)..."    
    sudo apt-get install -y sbcl
else
    echo "SBCL (Lisp) is already installed."
fi
