Inputs:
	$Input: TensorT[$$Dimensions]
	$Target: TensorT[$$Dimensions]

Outputs:
	$Loss: ScalarT

Parameters:
    $$C1:
	$$C2:
	$$Dimensions: SizeListT[]

Array:
	$Window:

AllowDynamicDimensions: True

==============================================

def _ssim(img1, img2, window, window_size, channel, size_average = True):
mu1 = F.conv2d(img1, window, padding = window_size//2, groups = channel)
mu2 = F.conv2d(img2, window, padding = window_size//2, groups = channel)

mu1_sq = mu1^2
mu2_sq = mu2^2
mu1_mu2 = mu1*mu2

sigma1_sq = F.conv2d(img1^2, window, padding = window_size//2, groups = channel) - mu1_sq
sigma2_sq = F.conv2d(img2^2, window, padding = window_size//2, groups = channel) - mu2_sq
sigma12 = F.conv2d(img1*img2, window, padding = window_size//2, groups = channel) - mu1_mu2

C1 = 0.01^2
C2 = 0.03^2

ssim_map = ((2*mu1_mu2 + C1)*(2*sigma12 + C2))/((mu1_sq + mu2_sq + C1)*(sigma1_sq + sigma2_sq + C2))

if size_average:
	return ssim_map.mean()
else:
	return ssim_map.mean(1).mean(1).mean(1)

===============================================

Options[SSIM] = {
	"C1" -> 0.01^2, "C2" -> 0.03^2,
	"Window" -> GaussianMatrix[{{(11 - 1) / 2, (11 - 1) / 2}, 1.5}, Method -> "Gaussian"]
};
SSIM[img1_Image, img2_Image, OptionsPattern[]] := Module[
	{c1, c2, window, mx, my, vx, vy, cov, r},
	{c1, c2, window} = OptionValue[{"C1", "C2", "Window"}];
	mx = ImageCorrelate[img1, window, Padding -> None];
	my = ImageCorrelate[img2, window, Padding -> None];
	vx = ImageCorrelate[img1^2, window, Padding -> None] - mx^2;
	vy = ImageCorrelate[img2^2, window, Padding -> None] - my^2;
	cov = ImageCorrelate[img1 * img2, window, Padding -> None] - mx * my;
	r = (2mx * my + c1) / (mx^2 + my^2 + c1) * (2cov + c2) / (vx + vy + c2);
	Mean@ImageMeasurements[r, "Mean"]
];

===================================================

Writer: Function[
	input = GetInput["Input"];
	target = GetInput["Target"];
	
	
	
	SetOutput["Loss", loss];
]

IsLoss: True

Suffix: "Layer"