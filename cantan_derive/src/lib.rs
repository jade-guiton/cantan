extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;


#[proc_macro_derive(Trace)]
pub fn collect_derive(input: TokenStream) -> TokenStream {
	let ast: syn::DeriveInput = syn::parse(input).unwrap();
	
	let name = &ast.ident;
	let instr = match ast.data {
		syn::Data::Struct(s) => {
			match s.fields {
				syn::Fields::Named(named) => {
					let mut instr = vec![];
					for field in named.named {
						let ident = field.ident.unwrap();
						instr.push(quote!{ self.#ident.trace(ctx); });
					}
					instr
				},
				syn::Fields::Unnamed(unnamed) => {
					let mut instr = vec![];
					for (i, _field) in unnamed.unnamed.iter().enumerate() {
						let idx = syn::Index { index: i as u32, span: quote::__private::Span::call_site() };
						instr.push(quote!{ self.#idx.trace(ctx); });
					}
					instr
				},
				syn::Fields::Unit => vec![],
			}
		},
		syn::Data::Enum(e) => {
			let mut branches = vec![];
			for v in e.variants {
				let ident = v.ident;
				let branch = match v.fields {
					syn::Fields::Named(named) => {
						let mut fields = vec![];
						let mut instr = vec![];
						for field in named.named {
							let ident = field.ident.unwrap();
							fields.push(ident.clone());
							instr.push(quote!{ #ident.trace(ctx); });
						}
						quote! { #name::#ident { #(#fields),* } => { #(#instr)* } }
					},
					syn::Fields::Unnamed(unnamed) => {
						let mut fields = vec![];
						let mut instr = vec![];
						for (i, _field) in unnamed.unnamed.iter().enumerate() {
							let ident = syn::Ident::new(&format!("__{}", i), quote::__private::Span::call_site());
							fields.push(ident.clone());
							instr.push(quote!{ #ident.trace(ctx); });
						}
						quote! { #name::#ident(#(#fields),*) => { #(#instr)* } }
					},
					syn::Fields::Unit => quote! { #name::#ident => {} },
				};
				branches.push(branch);
			}
			vec![quote!{
				match self {
					#(#branches)*
				}
			}]
		},
		_ => {
			unimplemented!("WIP - Collecting {}", name);
		}
	};
	let gen = quote! {
		unsafe impl Trace for #name {
			unsafe fn trace(&self, ctx: crate::gc::TraceCtx) {
				#(#instr)*
			}
		}
	};
	gen.into()
}
