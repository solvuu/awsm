# Vendor Files

Files in this directory are copied from external sources. We describe
how each directory or file was obtained below:

## `botocore/` and `aws4_testsuite`
```bash
export BOTOCORE_VERSION=1.24.40
wget https://github.com/boto/botocore/archive/$BOTOCORE_VERSION.tar.gz
tar xzf $BOTOCORE_VERSION.tar.gz
rm -f $BOTOCORE_VERSION.tar.gz

mkdir -p botocore/botocore
cd botocore-$BOTOCORE_VERSION
cp -Rpi botocore/data ../botocore/botocore/
cp CONTRIBUTING.rst LICENSE.txt NOTICE README.rst ../botocore/

mkdir aws4_testsuite
cp tests/unit/auth/aws4_testsuite/*/* aws4_testsuite

cd ..
rm -rf botocore-$BOTOCORE_VERSION
```

We really need only `botocore/botocore/data`, so don't retain most
other files. We do keep a few additional files so the original source
is clear.
