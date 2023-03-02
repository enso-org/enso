/** @type {import('tailwindcss').Config} */
module.exports = {
	content: ['./src/**/*.{js,ts,jsx,tsx}'],
	theme: {
			extend: {
				colors: {
					"label-bg": "#3e515f14",
					"perm-owner": "#51626e",
					"perm-admin": "#e06a50",
					"perm-edit": "#f0a042",
					"perm-read": "#a2c53e",
					"perm-read-no-exec": "#a2c53e85",
					"perm-exec": "#2d96c3",
					"perm-none": "#3e515f14",
				},
			},
	},
	plugins: [],
}
