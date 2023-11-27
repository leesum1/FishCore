add_rules("mode.release", "mode.debug")


if is_mode("release") then
    set_optimize("fastest")
--     set_policy("build.optimization.lto", true)
--     set_policy("build.sanitizer.address", true)
end

if is_mode("debug") then
    set_symbols("debug")
    set_optimize("none")
    set_policy("build.sanitizer.address", true)
end

add_requires("cli11",{system = false})
add_requires("assert",{system = true})
add_requires("elfio",{system = false})
add_requires("libsdl",{system = false})
add_requires("readerwriterqueue",{system = false})
add_requires("spdlog", {system = false})

set_policy("build.warning", true)
-- set_warnings("all", "extra")

-- 设置 C++20 标准
set_languages("cxx20")


add_rules("plugin.compile_commands.autoupdate", {outputdir = "."})
target("Vtop")
    add_rules("verilator.binary")
    set_toolchains("@verilator")
    add_files("src/*.cpp")
    add_files("vsrc/*.sv")
    add_values("verilator.flags","--top","FishSoc")
    add_values("verilator.flags","--trace-fst")
--     add_values("verilator.flags","--threads","2")
    add_includedirs("src/include/")
    add_packages("cli11","assert","elfio","libsdl","readerwriterqueue","spdlog")
    add_links("rv64emu_cbinding")

-- for _, file in ipairs(os.files("test/*.cpp")) do
--     local name = path.basename(file)
--     target(name)
--         set_kind("binary")
--         set_default(false)
--         add_files(file)
--         add_includedirs("src/include/")
--         add_packages("catch2","assert","elfio")
--         add_tests("default")
--
-- end

--
-- If you want to known more usage about xmake, please see https://xmake.io
--
-- ## FAQ
--
-- You can enter the project directory firstly before building project.
--
--   $ cd projectdir
--
-- 1. How to build project?
--
--   $ xmake
--
-- 2. How to configure project?
--
--   $ xmake f -p [macosx|linux|iphoneos ..] -a [x86_64|i386|arm64 ..] -m [debug|release]
--
-- 3. Where is the build output directory?
--
--   The default output directory is `./build` and you can configure the output directory.
--
--   $ xmake f -o outputdir
--   $ xmake
--
-- 4. How to run and debug target after building project?
--
--   $ xmake run [targetname]
--   $ xmake run -d [targetname]
--
-- 5. How to install target to the system directory or other output directory?
--
--   $ xmake install
--   $ xmake install -o installdir
--
-- 6. Add some frequently-used compilation flags in xmake.lua
--
-- @code
--    -- add debug and release modes
--    add_rules("mode.debug", "mode.release")
--
--    -- add macro definition
--    add_defines("NDEBUG", "_GNU_SOURCE=1")
--
--    -- set warning all as error
--    set_warnings("all", "error")
--
--    -- set language: c99, c++11
--    set_languages("c99", "c++11")
--
--    -- set optimization: none, faster, fastest, smallest
--    set_optimize("fastest")
--
--    -- add include search directories
--    add_includedirs("/usr/include", "/usr/local/include")
--
--    -- add link libraries and search directories
--    add_links("tbox")
--    add_linkdirs("/usr/local/lib", "/usr/lib")
--
--    -- add system link libraries
--    add_syslinks("z", "pthread")
--
--    -- add compilation and link flags
--    add_cxflags("-stdnolib", "-fno-strict-aliasing")
--    add_ldflags("-L/usr/local/lib", "-lpthread", {force = true})
--
-- @endcode
--

