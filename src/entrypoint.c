// We need to forward routine registration from C to Rust
// to avoid the linker removing the static library.

void R_init_sumo_extendr(void *dll);

void R_init_sumo(void *dll) {
    R_init_sumo_extendr(dll);
}
