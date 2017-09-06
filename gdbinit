set print pretty

# Linaro ART development
file /data/workspace/linaro/out/soong/host/linux-x86/bin/dex2oat
run --runtime-arg -Xms64m --runtime-arg -Xmx512m --dump-passes --dump-timing \
     --dex-file=/home/xuezho01/workspace/linaro/xueliang_test/MyArray.dex    \
     --oat-file=/home/xuezho01/workspace/linaro/xueliang_test/MyArray.oat    \
     --android-root=/data/workspace/linaro/out/target/product/angler/system \
     --runtime-arg -Xnorelocate \
     --instruction-set=arm64 --instruction-set-features=default -j1 \
     --dump-cfg=art.cfg

# loop unrolling factor
break art::HLoopOptimization::GetUnrollingFactor
break art::HLoopOptimization::Vectorize
